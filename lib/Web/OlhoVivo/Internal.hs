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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.OlhoVivo.Internal
  where

import Control.Exception (Exception, throwIO)
import Control.Lens ((^.), (^?))
import Data.Aeson (FromJSON, ToJSON, Result(..), Object, fromJSON)
import Data.Aeson.TH (Options(..), defaultOptions)
import qualified Data.Aeson.TH as Aeson (deriveJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Default (Default(..))
import Data.Text (Text, pack)
import Data.Typeable (Typeable)
import Network.Wreq (asJSON, defaults, responseBody)
import Network.Wreq.Lens (statusCode)
import Network.Wreq.Types (Options(..))
import Network.Wreq.Session hiding (withSession)
import qualified Network.Wreq.Session as Wreq (withSession)

import Web.OlhoVivo.TH (deriveJSON, dropPrefix)

-- Core types
-------------------------------------------------------------------------------

-- |
-- Options for the Olho Vivo API Client. On most cases the 'Default' instance
-- should be ok
data OlhoVivoApiOptions = OlhoVivoApiOptions { olhovivoApiVersion :: String
                                             , olhovivoApiBaseUrl :: String
                                             }
  deriving(Eq, Ord, Show)

instance Default OlhoVivoApiOptions where
  def = OlhoVivoApiOptions
            { olhovivoApiBaseUrl = "http://api.olhovivo.sptrans.com.br/v"
            , olhovivoApiVersion = "0"
            }

-- |
-- A line code is required to fetch the positions for a given line. There are
-- several ways to fetch them, but you'll be better off seeking for existing
-- line code scrapping solutions, such as the scripts I've written in:
-- <https://github.com/yamadapc/node-olhovivo node-olhovivo>
-- .
-- Other aliases for codes are the same.
newtype LineCode = LineCode Int
  deriving(Eq, Ord, Show, FromJSON, ToJSON, Typeable)
newtype ExpressLaneCode = ExpressLaneCode Int
  deriving(Eq, Ord, Show, FromJSON, ToJSON, Typeable)
newtype StopCode = StopCode Int
  deriving(Eq, Ord, Show, FromJSON, ToJSON, Typeable)

-- |
-- Line information, as returned by /Linha/Buscar
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

deriveJSON "olhovivoLine" ''OlhoVivoLine

-- |
-- Stop information, as returned by /Parada/Buscar.../
data OlhoVivoStop =
    OlhoVivoStop { olhovivoStopCodigoParada :: StopCode
                 , olhovivoStopNome :: String
                 , olhovivoStopEndereco :: String
                 , olhovivoStopLatitude :: Double
                 , olhovivoStopLongitude :: Double
                 }
  deriving(Eq, Ord, Show)

deriveJSON "olhovivoStop" ''OlhoVivoStop

-- |
-- Express Lane information, as returned by /Corredor
data OlhoVivoExpressLane =
    OlhoVivoExpressLane { olhovivoExpressLaneCodCorredor :: ExpressLaneCode
                        , olhovivoExpressLaneNome :: String
                        }
  deriving(Eq, Ord, Show)

deriveJSON "olhovivoExpressLane" ''OlhoVivoExpressLane

-- |
-- Positional information, as returned by /Posicao
data OlhoVivoPosition = OlhoVivoPosition { olhovivoPositionP :: String
                                         , olhovivoPositionA :: Bool
                                         , olhovivoPositionPy :: Double
                                         , olhovivoPositionPx :: Double
                                         }
  deriving(Eq, Ord, Show)

let prefix = "olhovivoPosition" :: String
  in Aeson.deriveJSON
         defaultOptions { fieldLabelModifier = map toLower . dropPrefix prefix }
         ''OlhoVivoPosition

-- |
-- Custom exception type for @olhovivo@. Errors are thrown using 'throw' and
-- 'throwIO' from 'Control.Exception'
-- When an unknown error happens, the status code and response body are passed
-- on through the 'UnknownError' constructor
data OlhoVivoError = AuthenticationError
                   | UnknownError Int String
  deriving(Show, Typeable)

instance Exception OlhoVivoError

-- Core Logic
-------------------------------------------------------------------------------

-- |
-- Authenticates an existing Wreq `Session`. Throws `AuthenticationError` on
-- failure
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
-- Sessions
withSession :: OlhoVivoApiOptions -> Text -> (Session -> IO a) -> IO a
withSession opts token action = Wreq.withSession $ \session -> do
    authenticateSession opts token session
    action session

-- |
-- Performs a properly configured GET request to the Olho Vivo API
olhoVivoGet :: FromJSON a => Session -> OlhoVivoApiOptions -> String
            -> Network.Wreq.Types.Options -> IO a
olhoVivoGet session opts endpoint reqOpts = do
    let url = urlForEndpoint opts endpoint
    json <- asJSON =<< getWith reqOpts session url
    return $ json ^. responseBody

-- |
-- Gets the target URL for a given endpoint and a set of options
urlForEndpoint :: OlhoVivoApiOptions -> String -> String
urlForEndpoint opts endpoint =
    olhovivoApiBaseUrl opts ++ olhovivoApiVersion opts ++ endpoint
