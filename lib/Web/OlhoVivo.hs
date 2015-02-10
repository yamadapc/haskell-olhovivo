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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Web.OlhoVivo
    (
      authenticateSession
    , withSession
    , queryLines
    , queryStops
    , queryPositions
    , listExpressLanes
    -- Types
    , OlhoVivoLine(..)
    , OlhoVivoStop(..)
    , OlhoVivoExpressLane(..)
    , OlhoVivoPosition(..)
    , OlhoVivoError(..)
    , LineCode(..)
    , ExpressLaneCode(..)
    , StopCode(..)
    , Text(..)
    -- Re-exports
    , Default(..)
    , Session(..)
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

queryLines :: Session -> OlhoVivoApiOptions -> Text -> IO [OlhoVivoLine]
queryLines session opts q =
    let reqOpts = defaults { params = [ ("termosBusca", q) ] }
      in olhoVivoGet session opts "/Linha/Buscar" reqOpts

listExpressLanes :: Session -> OlhoVivoApiOptions -> IO [OlhoVivoExpressLane]
listExpressLanes session opts = olhoVivoGet session opts "/Corredor" defaults

queryPositions :: Session -> OlhoVivoApiOptions -> LineCode
                   -> IO [OlhoVivoPosition]
queryPositions session opts lineCode = do
    let reqOpts = defaults { params = [ ("codigoLinha", pack $ show lineCode) ] }
    res <- olhoVivoGet session opts "/Posicao" reqOpts :: IO Object
    case fromJSON <$> HashMap.lookup ("vs" :: Text) res of
        Just (Success ps) -> return ps
        Just (Error err) -> fail err
        Nothing -> fail "Unable to parse the Olho Vivo API's response"

-- |
-- @queryStops@
-- An overloaded function for searching for stops, by 'Text', 'LineCode',
-- 'ExpressLaneCode' or any combination of them and its variadic boilerplate
class StopsQuery q where
    sqToParams :: q -> [(Text, Text)]
    sqEndpoint :: q -> String

    queryStops :: StopsQuery q => Session -> OlhoVivoApiOptions -> q -> IO [OlhoVivoStop]
    queryStops session opts q =
        let reqOpts = defaults { params = sqToParams q }
          in olhoVivoGet session opts (sqEndpoint (undefined :: q)) reqOpts

instance StopsQuery Text where
    sqToParams q = [("termosBusca", q)]
    sqEndpoint = const "/Parada/Buscar"

instance StopsQuery LineCode where
    sqToParams (LineCode l) = [("codigoLinha", pack (show l))]
    sqEndpoint = const "/Parada/BuscarParadasPorLinha"

instance StopsQuery ExpressLaneCode where
    sqToParams (ExpressLaneCode e) = [("codigoCorredor", pack (show e))]
    sqEndpoint = const "/Parada/BuscarParadasPorCorredor"
