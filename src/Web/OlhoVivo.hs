{-# LANGUAGE OverloadedStrings #-}
module Web.OlhoVivo where

import Control.Lens
import Data.ByteString.Lazy (ByteString)
import Data.Default (Default(..))
import Data.Text (Text)
import Network.Wreq (defaults, responseBody)
import Network.Wreq.Types (Options(..))
import Network.Wreq.Session

data OlhoVivoApiOptions = OlhoVivoApiOptions { olhoVivoApiVersion :: String
                                             , olhoVivoApiBaseUrl :: String
                                             }

instance Default OlhoVivoApiOptions where
  def = OlhoVivoApiOptions
            { olhoVivoApiBaseUrl = "http://api.olhovivo.sptrans.com.br/v"
            , olhoVivoApiVersion = "0"
            }

-- |
-- Authenticates a Wreq 'Session' with the OlhoVivo API given a set of API
-- Options and an application token.
newOlhoVivoApi :: Session
               -- ^ The wreq session to authenticate
               -> OlhoVivoApiOptions
               -- ^ Version and base URL options
               -> Text
               -- ^ The API token to authenticate with
               -> IO Bool
               -- ^ Whether the authentication was successful
newOlhoVivoApi session opts token = do
    let host     = olhoVivoApiBaseUrl opts ++ olhoVivoApiVersion opts
        endpoint = "/Login/Autenticar"
        reqOpts  = defaults { params = [ ("token", token)
                                       ]
                            }
    res <- postWith reqOpts session (host ++ endpoint) ("" :: ByteString)
    case res ^? responseBody of
       Just "true" -> return True
       Just "false" -> return False
       Nothing -> return False
