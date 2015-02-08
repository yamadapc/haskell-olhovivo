-- An API wrapper for the SPTrans Olho Vivo API.
-- Copyright (C) 2015 Pedro Tacla Yamada
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program; if not, write to the Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.OlhoVivo
  where

import Control.Applicative ((<$>))
import Control.Exception (Exception, throwIO)
import Control.Lens ((^.), (^?))
import Data.Aeson (FromJSON, Result(..), Object, fromJSON)
import Data.Aeson.TH (Options(..), defaultOptions)
import qualified Data.Aeson.TH as Aeson (deriveJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Default (Default(..))
import qualified Data.HashMap.Strict as HashMap (lookup)
import Data.Typeable (Typeable)
import Data.Text (Text, pack)
import Network.Wreq (asJSON, defaults, responseBody)
import Network.Wreq.Lens (statusCode)
import Network.Wreq.Types (Options(..))
import Network.Wreq.Session hiding (withSession)
import qualified Network.Wreq.Session as Wreq (withSession)

import Web.OlhoVivo.TH

data OlhoVivoApiOptions = OlhoVivoApiOptions { olhovivoApiVersion :: String
                                             , olhovivoApiBaseUrl :: String
                                             }
  deriving(Eq, Ord, Show)

type LineCode = Int

data OlhoVivoLine =
    OlhoVivoLineSummary { olhovivoLineCodigoLinha :: LineCode
                        , olhovivoLineCircular :: Bool
                        , olhovivoLineLetreiro :: String
                        , olhovivoLineSentido :: Int
                        , olhovivoLineTipo :: Int
                        , olhovivoLineDenominacaoTPTS :: String
                        , olhovivoLineDenominacaoTSTP :: String
                        , olhovivoLineInformacoes :: Maybe String
                        }
  deriving(Eq, Ord, Show)

$(deriveJSON "olhovivoLine" ''OlhoVivoLine)

data OlhoVivoStop =
    OlhoVivoStop { olhovivoStopCodigoParada :: Int
                 , olhovivoStopNome :: String
                 , olhovivoStopEndereco :: String
                 , olhovivoStopLatitude :: Double
                 , olhovivoStopLongitude :: Double
                 }
  deriving(Eq, Ord, Show)

$(deriveJSON "olhovivoStop" ''OlhoVivoStop)

data OlhoVivoExpressLane =
    OlhoVivoExpressLane { olhovivoExpressLaneCodCorredor :: Int
                        , olhovivoExpressLaneNome :: String
                        }
  deriving(Eq, Ord, Show)

$(deriveJSON "olhovivoExpressLane" ''OlhoVivoExpressLane)

data OlhoVivoPosition = OlhoVivoPosition { olhovivoPositionP :: String
                                         , olhovivoPositionA :: Bool
                                         , olhovivoPositionPy :: Double
                                         , olhovivoPositionPx :: Double
                                         }
  deriving(Eq, Ord, Show)

$(let prefix = "olhovivoPosition" :: String
  in Aeson.deriveJSON
         defaultOptions { fieldLabelModifier = map toLower . dropPrefix prefix }
         ''OlhoVivoPosition)

instance Default OlhoVivoApiOptions where
  def = OlhoVivoApiOptions
            { olhovivoApiBaseUrl = "http://api.olhovivo.sptrans.com.br/v"
            , olhovivoApiVersion = "0"
            }

data OlhoVivoError = AuthenticationError
                   | UnknownError Int String
  deriving(Show, Typeable)

instance Exception OlhoVivoError

-- |
-- Authenticates an existing Wreq `Session`. Throws `AuthenticationError` on
-- failure.
authenticateSession :: OlhoVivoApiOptions -> Text -> Session -> IO ()
authenticateSession opts token session = do
    let url = urlForEndpoint opts "/Login/Autenticar"
        reqOpts = defaults { params = [ ("token", token)]}
    res <- postWith reqOpts session url ("" :: ByteString)
    -- print $ res ^. statusCode
    case res ^? responseBody of
       Just "true" -> return ()
       Just "false" -> throwIO AuthenticationError
       _ -> throwIO AuthenticationError

-- |
-- Like `Network.Wreq.Session.withSession` but for authenticated Olho Vivo API
-- Sessions.
withSession :: OlhoVivoApiOptions -> Text -> (Session -> IO a) -> IO a
withSession opts token action = Wreq.withSession $ \session -> do
    authenticateSession opts token session
    action session

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
    let url = urlForEndpoint opts "/Login/Autenticar"
        reqOpts = defaults { params = [ ("token", token)
                                      ]
                            }
    res <- postWith reqOpts session url ("" :: ByteString)
    case res ^? responseBody of
       Just "true" -> return True
       Just "false" -> return False
       _ -> return False

olhoVivoLines :: Session -> OlhoVivoApiOptions -> Text -> IO [OlhoVivoLine]
olhoVivoLines session opts q =
    let reqOpts = defaults { params = [ ("termosBusca", q) ] }
      in olhoVivoGet session opts "/Linha/Buscar" reqOpts

olhoVivoStops :: Session -> OlhoVivoApiOptions -> Text -> IO [OlhoVivoStop]
olhoVivoStops session opts q =
    let reqOpts = defaults { params = [ ("termosBusca", q) ] }
      in olhoVivoGet session opts "/Parada/Buscar" reqOpts

olhoVivoExpressLanes :: Session -> OlhoVivoApiOptions -> IO [OlhoVivoExpressLane]
olhoVivoExpressLanes session opts =
      olhoVivoGet session opts "/Corredor" defaults


olhoVivoStopsInLine :: Session -> OlhoVivoApiOptions -> LineCode -> IO [OlhoVivoStop]
olhoVivoStopsInLine session opts lineCode =
    let reqOpts = defaults { params = [ ("codigoLinha", pack (show lineCode)) ] }
      in olhoVivoGet session opts "/Parada/BuscarParadasPorLinha" reqOpts

olhoVivoStopsInExpressLane :: Session -> OlhoVivoApiOptions -> Int
                           -> IO [OlhoVivoStop]
olhoVivoStopsInExpressLane session opts expressLaneCode =
    let reqOpts = defaults { params =
                                 [ ("codigoCorredor" , pack $ show expressLaneCode)
                                 ]
                           }
      in olhoVivoGet session opts "/Parada/BuscarParadasPorCorredor" reqOpts

olhoVivoLinePositions :: Session -> OlhoVivoApiOptions -> LineCode
                  -> IO [OlhoVivoPosition]
olhoVivoLinePositions session opts lineCode = do
    let reqOpts = defaults { params = [ ("codigoLinha", pack $ show lineCode) ]
                           }
    res <- olhoVivoGet session opts "/Posicao" reqOpts :: IO Object
    case fromJSON <$> HashMap.lookup ("vs" :: Text) res of
        Just (Success ps) -> return ps
        Just (Error err) -> fail err
        Nothing -> fail "Unable to parse the Olho Vivo API's response"

urlForEndpoint :: OlhoVivoApiOptions -> String -> String
urlForEndpoint opts endpoint =
    olhovivoApiBaseUrl opts ++ olhovivoApiVersion opts ++ endpoint

olhoVivoGet :: FromJSON a => Session -> OlhoVivoApiOptions -> String
            -> Network.Wreq.Types.Options -> IO a
olhoVivoGet session opts endpoint reqOpts = do
    let url = urlForEndpoint opts endpoint
    json <- asJSON =<< getWith reqOpts session url
    return $ json ^. responseBody
