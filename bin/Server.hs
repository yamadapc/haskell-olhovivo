{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Control.Monad
import Data.Monoid ((<>))
import Data.Text as T (Text, pack, unpack)
import qualified Data.Text.IO as T
import Network.WebSockets as WS
import Web.OlhoVivo

type ServerState = [Client]
type Client = WS.Connection

newServerState :: ServerState
newServerState = []

broadcast :: Text -> ServerState -> IO ()
broadcast message clients = do
    T.putStrLn $ "Sending: \"" <> message <> "\" to " <> pack (show (length clients))
    forM_ clients $ \conn -> WS.sendTextData conn message

main :: IO ()
main = do
    state <- newMVar newServerState
    putStrLn "Starting server..."
    WS.runServer "0.0.0.0" 9160 $ application state

application :: MVar ServerState -> WS.ServerApp
application state pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30

    WS.sendTextData conn ("{\"type\": \"handshake\"}" :: Text)
    modifyMVar_ state $ \cs -> return $ conn:cs
    forever $ do
        msg <- WS.receiveData conn
        T.putStrLn msg
        broadcast msg =<< readMVar state
