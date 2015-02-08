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
{-# LANGUAGE OverloadedStrings #-}
module Web.OlhoVivo
    (
      authenticateSession
    , withSession
    , olhoVivoLines
    , olhoVivoLinePositions
    , olhoVivoExpressLanes
    , olhoVivoStops
    , olhoVivoStopsInLine
    , olhoVivoStopsInExpressLane
    -- Types
    , OlhoVivoLine(..)
    , OlhoVivoStop(..)
    , OlhoVivoExpressLane(..)
    , OlhoVivoPosition(..)
    , OlhoVivoError(..)
    -- Re-exports
    , Default(..)
    )
  where

import Control.Applicative ((<$>))
import Control.Exception (Exception, throwIO)
import Control.Lens ((^.), (^?))
import Data.Aeson (FromJSON, Result(..), Object, fromJSON)
import Data.Aeson.TH (Options(..), defaultOptions)
import qualified Data.Aeson.TH as Aeson (deriveJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Default (Default(..))
import qualified Data.HashMap.Strict as HashMap (lookup)
import Data.Typeable (Typeable)
import Data.Text (Text, pack)
import Network.Wreq (asJSON, defaults, responseBody)
import Network.Wreq.Lens (statusCode)
import Network.Wreq.Types (Options(..))
import Network.Wreq.Session hiding (withSession)
import qualified Network.Wreq.Session as Wreq (withSession)

import Web.OlhoVivo.Internal

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
