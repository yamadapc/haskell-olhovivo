{-# LANGUAGE OverloadedStrings #-}
module Web.OlhoVivoSpec
  where

import Control.Lens ((^.))
import Data.Default (def)
import Data.String (fromString)
import Data.Text (Text)
import Network.Wreq (defaults)
import Network.Wreq.Lens (responseStatus, statusCode)
import Network.Wreq.Session hiding (withSession)
import qualified Network.Wreq.Session as Wreq (withSession)
import Network.Wreq.Types (Options(..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import System.Environment (getEnv)
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Web.OlhoVivo

token :: Text
token = unsafePerformIO $ fromString `fmap` getEnv "SPTRANS_TOKEN"

withTestSession = withSession def token

spec :: Spec
spec = do
    describe "authenticateSession" $
        it "authenticates a given HTTP session" $
            Wreq.withSession $ \session -> do
                authenticateSession def token session
                sessionIsAuthenticated session

    describe "withSession" $
        it "creates a new authenticated olhovivo `Session` object" $
           withSession def token $ \session ->
               sessionIsAuthenticated session

    describe "olhoVivoLine" $
        it' "makes a query for lines matching a string" $ \session -> do
            res <- olhoVivoLines session def "bandeira"
            any (\l -> olhovivoLineLetreiro l == "6262" &&
                       olhovivoLineTipo l == 10) res
                `shouldBe` True

    describe "olhoVivoStops" $
        it' "makes a query for stops matching a string" $ \session -> do
            res <- olhoVivoStops session def "Afonso"
            any (\s -> olhovivoStopEndereco s ==
                       "R ARMINDA/ R BALTHAZAR DA VEIGA") res
                `shouldBe` True

    describe "olhoVivoExpressLanes" $
        it' "gets all express lanes in the city" $ \session -> do
            res <- olhoVivoExpressLanes session def
            any (\s -> olhovivoExpressLaneNome s == "Campo Limpo") res
                `shouldBe` True

  where
    it' title action = it title (withTestSession action)

sessionIsAuthenticated :: Session -> IO ()
sessionIsAuthenticated session = do
    res <- get session (urlForEndpoint def "/Linha/Buscar?termosBusca=bandeira")
    res ^. responseStatus . statusCode `shouldBe` 200
