{-# LANGUAGE OverloadedStrings #-}
module Web.OlhoVivoSpec
  where

import Data.Default (def)
import Data.String (fromString)
import Network.Wreq (defaults)
import Network.Wreq.Session
import Network.Wreq.Types (Options(..))
import Test.Hspec (Spec, describe, it, shouldBe)
import System.Environment (getEnv)
import System.IO
import Web.OlhoVivo

spec :: Spec
spec = do
    describe "newOlhoVivoApi" $
        it "authenticates a wreq session with the Olho Vivo API" $
            withSession $ \session -> do
                token <- fromString `fmap` getEnv "SPTRANS_TOKEN"
                res <- newOlhoVivoApi
                           session
                           def
                           token
                res `shouldBe` True

    describe "olhoVivoLine" $
        it "makes a query for lines matching a string" $
            withSession $ \session -> do
                token <- fromString `fmap` getEnv "SPTRANS_TOKEN"
                res <- newOlhoVivoApi session def token

                res' <- olhoVivoLines session def "bandeira"
                any (\l -> olhovivoLineLetreiro l == "6262" &&
                           olhovivoLineTipo l == 10) res'
                    `shouldBe` True

    -- describe "olhoVivoLinhaDetails" $
    --     it "fetches details relative to a certain line" $
    --         withSession $ \session -> do
    --             token <- fromString `fmap` getEnv "SPTRANS_TOKEN"
    --             res <- newOlhoVivoApi session def token

    --             let endpoint = "/Linha/CarregarDetalhes"
    --                 reqOpts = defaults { params = [ ("codigoLinha", "906") ]
    --                                    }
    --             res <- getWith reqOpts session (urlForEndpoint def endpoint)
    --             hPrint stderr res

    --             return ()

    describe "olhoVivoStops" $
        it "makes a query for stops matching a string" $
            withSession $ \session -> do
                token <- fromString `fmap` getEnv "SPTRANS_TOKEN"
                res <- newOlhoVivoApi session def token

                res' <- olhoVivoStops session def "Afonso"
                any (\s -> olhovivoStopEndereco s ==
                               "R ARMINDA/ R BALTHAZAR DA VEIGA") res'
                    `shouldBe` True

    describe "olhoVivoExpressLanes" $
        it "gets all express lanes in the city" $
            withSession $ \session -> do
                token <- fromString `fmap` getEnv "SPTRANS_TOKEN"
                res <- newOlhoVivoApi session def token

                res' <- olhoVivoExpressLanes session def
                any (\s -> olhovivoExpressLaneNome s ==
                               "Campo Limpo") res'
                    `shouldBe` True

    -- describe "olhoVivoLinePositions" $
    --     it "gets all bus positions for a line code" $
    --         withSession $ \session -> do
    --             token <- fromString `fmap` getEnv "SPTRANS_TOKEN"
    --             res <- newOlhoVivoApi session def token

    --             res' <- olhoVivoLinePositions session def 906
    --             hPrint stderr res'
