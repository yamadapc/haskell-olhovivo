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
module Web.OlhoVivo.TH
  where

import Data.Aeson.TH (Options(..), defaultOptions)
import qualified Data.Aeson.TH as Aeson (deriveJSON)
import Data.Char (toLower)
import Language.Haskell.TH (Name, Q, Dec)

-- |
-- Derives a JSON instance for a ADT which has all of it's keys prefixed by a
-- certain prefix
deriveJSON :: String -> Name -> Q [Dec]
deriveJSON prefix = Aeson.deriveJSON opts
  where
    opts = defaultOptions { fieldLabelModifier = drop $ length prefix
                          , constructorTagModifier = \(c:cs) -> toLower c : cs
                          }

-- |
-- Drops a prefix list from another list. Assumes the target list has the
-- prefix and `length` bigger or equal to the prefix's.
dropPrefix :: [a] -> [a] -> [a]
dropPrefix prefix = drop $ length prefix
