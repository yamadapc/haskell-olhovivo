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
import Network.Wreq.Session
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

    _ <- forkIO $ withSession $ \session -> do
        r <- newOlhoVivoApi session def token
        putStrLn "Authenticated with the olhovivo API"
        if r
            then do
                lineCodes <- map olhovivoLineCodigoLinha <$>
                              olhoVivoLines session def "azevedo"
                putStrLn $
                    "Fetched " ++ show (length lineCodes) ++ " available lines"

                let loop lineCode = do
                        positions <- olhoVivoLinePositions session def lineCode
                        print positions
                        mapConcurrently
                            (atomically . writeTChan (serverPublishChan state))
                            (zip (repeat lineCode) positions)
                        threadDelay (1000 * 1000)
                        loop lineCode

                void $ mapConcurrently loop lineCodes
            else undefined
    WS.runServer "0.0.0.0" 9160 $ application state

-- |
-- The application's accept handler. Adds a client into the global state,

application :: ServerState -> WS.ServerApp
application state pending = WS.acceptRequest pending >>= stablishConnection
  where
    authErrorMessage :: String
    authErrorMessage = "{\"type\":\"error\",\"message\":" ++
                       "\"Client not authenticated\"}"
    stablishConnection conn = handshakeClient conn >>= \case
        Nothing -> do
            WS.sendTextData conn (pack authErrorMessage)
            stablishConnection conn
        Just client ->
            -- Add the client to the application state and ping it every 30s
            bracket_ (addClient client state) (removeClient client state) $ do
                localChan <- atomically $ dupTChan (serverPublishChan state)
                WS.forkPingThread conn 30
                forever $ do
                    (i, p) <- atomically $ readTChan localChan

                    let si = ByteString.pack (show i)
                        message = "{\"type\":\"position\",\"message\":" <>
                                  "{\"codigoLinha\":" <> si <> "," <>
                                  "\"posicao\":" <> Aeson.encode p <>
                                  " }}"

                    WS.sendTextData (snd client) message
