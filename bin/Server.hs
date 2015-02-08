{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
import Control.Applicative ((<$>), (<*>))
import Control.Exception (bracket_)
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import qualified Data.Aeson as Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as ByteString (pack)
import Data.Default (def)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text as T (Text, pack, unpack)
import qualified Data.Text.IO as T
import Network.WebSockets as WS
import Network.Wreq.Session hiding (withSession)
import qualified Network.Wreq.Session as Wreq (withSession)
import System.Environment (getEnv)
import System.Random
import Web.OlhoVivo

data ServerState =
    ServerState { serverClients :: MVar [Client]
                , serverPublishChan :: TChan (Int, OlhoVivoPosition)
                , serverKnownLines :: MVar [Int]
                }
 deriving(Eq)

type Client = (Text, WS.Connection)

newServerState :: IO ServerState
newServerState = ServerState <$> newMVar []
                             <*> atomically newTChan
                             <*> newMVar []

addClient :: Client -> ServerState -> IO ()
addClient client state = do
    let clientsMVar = serverClients state
    modifyMVar_ clientsMVar $ \clients ->
        return $ client:clients

removeClient :: Client -> ServerState -> IO ()
removeClient client state = do
    let clientsMVar = serverClients state
    modifyMVar_ clientsMVar $ \clients ->
        return $ filter (\client' -> fst client' /= fst client) clients

broadcastMessage clients message = forM_ clients $ \c ->
    WS.sendTextData (snd c) message

handshakeClient :: WS.Connection -> IO (Maybe Client)
handshakeClient conn = do
    message <- WS.receiveData conn
    case (message :: Text) of
        "validated-token" -> do
            stdGen <- getStdGen
            let (i, g) = random stdGen :: (Int, StdGen)
            setStdGen stdGen
            return $ Just ("validated-token-" <> pack (show i), conn)
        _ -> return Nothing

main :: IO ()
main = do
    token <- fromString <$> getEnv "SPTRANS_TOKEN"

    state <- newServerState
    putStrLn "Starting olhovivo transport thread..."

    _ <- forkIO $ Wreq.withSession $ \session -> do
        r <- newOlhoVivoApi session def token
        putStrLn "Authenticated with the olhovivo API"
        lineCodes <- fetchLineCodes session token
        listenForPositions session state lineCodes
    WS.runServer "0.0.0.0" 9160 $ application state

fetchLineCodes :: Session -> Text -> IO [Int]
fetchLineCodes session token = do
    r <- newOlhoVivoApi session def token
    if r
        then do
            lineCodes <- map olhovivoLineCodigoLinha <$>
                         olhoVivoLines session def "azevedo"
            putStrLn $ "Fetched " ++ show (length lineCodes) ++ " available lines"
            return lineCodes
        else fetchLineCodes session token

listenForPositions :: Session -> ServerState -> [Int] -> IO ()
listenForPositions session state = void . mapConcurrently loop
  where
    outputChan = serverPublishChan state
    loop lineCode = do
        positions <- olhoVivoLinePositions session def lineCode
        mapConcurrently
            (atomically . writeTChan outputChan)
            (zip (repeat lineCode) positions)
        threadDelay (1000 * 1000)
        loop lineCode

-- |
-- The application's accept handler. Adds a client into the global state,
application :: ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    handshakeClient conn >>= \case
        Nothing -> WS.sendTextData conn authErrorMessage
        Just client ->
            -- Add the client to the application state and ping it every 30s
            bracket_ (addClient client state) (removeClient client state) $ do
                localChan <- atomically $ dupTChan (serverPublishChan state)
                WS.forkPingThread conn 30
                forever $ do
                    message <- atomically $ readTChan localChan
                    WS.sendTextData (snd client) (serializePosition message)
  where
    authErrorMessage = pack ("{\"type\":\"error\",\"message\":" ++
                             "\"Client not authenticated\"}")
    serializePosition (lineCode, position) =
        "{\"type\":\"position\",\"message\":" <>
        "{\"codigoLinha\":" <> ByteString.pack (show lineCode) <> "," <>
        "\"posicao\":" <> Aeson.encode position <>
        " }}"
