{-# LANGUAGE OverloadedStrings #-}
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.Text (Text, unpack)
import qualified Data.Text as T (null)
import qualified Data.Text.IO as T
import Network.WebSockets as WS
import Network.Socket (withSocketsDo)
import System.IO
import Web.OlhoVivo

main :: IO ()
main = do
    putStrLn "Connecting to server..."
    withSocketsDo $ WS.runClient "0.0.0.0" 9160 "/" application

application :: WS.ClientApp ()
application conn = do
    hSetBuffering stdin LineBuffering
    putStrLn "Connected!"

    _ <- forkIO $ forever $ do
        d <- WS.receiveData conn
        liftIO $ T.putStrLn d

    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop
    loop
