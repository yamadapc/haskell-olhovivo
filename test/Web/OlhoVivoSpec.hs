{-# LANGUAGE OverloadedStrings #-}
module Web.OlhoVivoSpec
  where

import Data.Default (def)
import Data.String (fromString)
import Network.Wreq.Session
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

    describe "olhoVivoLinhas" $
        it "makes a query for lines matching a string" $
            withSession $ \session -> do
                token <- fromString `fmap` getEnv "SPTRANS_TOKEN"
                res <- newOlhoVivoApi
                           session
                           def
                           token
                res' <- olhoVivoLinhas session def "bandeira"
                any (\l -> olhovivoLineLetreiro l == "6262" &&
                           olhovivoLineTipo l == 10) res'
                    `shouldBe` True
