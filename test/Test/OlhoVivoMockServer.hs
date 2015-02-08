{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
import Control.Applicative ((<$>))
import Control.Monad (join)
import Data.Aeson hiding (json)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text (Text, intercalate)
import Data.Text.Lazy.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy (Text)
import NeatInterpolation
import Network.HTTP.Types (status401, status404)
import Network.Wai (queryString, pathInfo)
import qualified Blaze.ByteString.Builder as Blaze (toLazyByteString)
import Web.Cookie
import Web.Scotty
import Web.Scotty.Trans (ActionT)

host :: Text
host = "http://api.olhovivo.sptrans.com.br/v0"

-- |
-- Our router logic and application container
server :: ScottyM ()
server = do
    post "/Login/Autenticar" login
    post "/Linha/Busca" $ requiresAuthentication queryLines

-- Entry point for easier testing
main :: IO ()
main = scotty 3000 server

-- POST /Login/Autenticar?token=:token
login :: ActionM ()
login = maybeQuery "token" >>= \case
    -- This is mimicking the production server's behaviour. I wouldn't design
    -- this system like this myself.
    -- There's a slight difference, because the production API responds with
    -- 500 when a request is made like: `/Login/Autenticar?token`
    -- This behaviour won't be reproduced here
    Nothing -> notFoundError
    Just token ->
        if validateToken token
           then do
             setCookie "apiCredentials" "bogus-hash"
             json True
           else json False
  where
    validateToken "valid-token" = True
    validateToken _ = False

-- GET /Linha/Busca?termosBusca=:termosBusca
queryLines :: ActionM ()
queryLines = maybeQuery "termosBusca" >>= \case
    Nothing -> notFoundError
    Just _ -> raw $ fromString $ [string|
{
  "CodigoLinha": 667,
  "Circular": true,
  "Letreiro": "1721",
  "Sentido": 1,
  "Tipo": 51,
  "DenominacaoTPTS": "TERM. BANDEIRA",
  "DenominacaoTSTP": "VILA EDE",
  "Informacoes": null
}
              |]

-- |
-- Tranforms a handler into a Authenticated handler (one that'll respond with
-- 401 if the request isn't "authenticated")
requiresAuthentication :: ActionM () -> ActionM ()
requiresAuthentication handler = isLoggedIn >>= \case
    True -> handler
    False -> unauthorizedError

-- |
-- Safe version of `Web.Scotty.param` possibly should be ditched in favor of
-- using `rescue`
maybeQuery :: ByteString -> ActionT Data.Text.Lazy.Text IO (Maybe ByteString)
maybeQuery name = do
    qs <- queryString <$> request
    return $ join $ lookup name qs

-- |
-- A helper to mimick the Olho Vivo API's Not Found error
notFoundError :: ActionM ()
notFoundError = do
    path <- ("/" <>) . intercalate "/" . pathInfo <$> request
    status status404
    json $ object ["Message" .= err path]
  where
    err path = "No HTTP resource was found that matches " <>
               "the request URI '" <> host <> path

-- |
-- A helper to mimick the Olho Vivo API's Unauthorized error
unauthorizedError :: ActionM ()
unauthorizedError = do
    status status401
    json $ object ["Message" .=
                   ("Authorization has been denied for this request" :: Text)]

-- |
-- Yields true if a reqeust is "authenticated"
isLoggedIn :: ActionM Bool
isLoggedIn = do
    mcookies <- getCookies
    case join (lookup "apiCredentials" <$> mcookies) of
        Just "bogus-hash" -> return True
        _ -> return False

-- Cookie handling functions
-- Thanks to https://gist.github.com/hdgarrood/7778032
-------------------------------------------------------------------------------

setCookie :: ByteString -> ByteString -> ActionM ()
setCookie k v = setHeader "Set-Cookie" (renderSetCookie' cookie)
  where
    renderSetCookie' = decodeUtf8 . Blaze.toLazyByteString . renderSetCookie
    cookie = def { setCookieName = k
                 , setCookieValue = v
                 }

getCookies :: ActionM (Maybe CookiesText)
getCookies = do
    rawCookies <- header "Cookie"
    return $ parseCookiesText . toStrict . encodeUtf8 <$> rawCookies

