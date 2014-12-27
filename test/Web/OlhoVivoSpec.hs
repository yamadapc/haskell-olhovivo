{-# LANGUAGE OverloadedStrings #-}
module Web.OlhoVivoSpec
  where

import Data.Default (def)
import Data.String (fromString)
import Network.Wreq.Session
import Test.Hspec (Spec, describe, it, shouldBe)
import System.Environment (getEnv)
import Web.OlhoVivo

spec :: Spec
spec =
    describe "newOlhoVivoApi" $
        it "authenticates a wreq session with the Olho Vivo API" $
            withSession $ \session -> do
                token <- fromString `fmap` getEnv "SPTRANS_TOKEN"
                res <- newOlhoVivoApi
                           session
                           def
                           token
                print res
                res `shouldBe` True
