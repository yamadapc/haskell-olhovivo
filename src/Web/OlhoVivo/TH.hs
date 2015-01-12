module Web.OlhoVivo.TH where

import Data.Aeson.TH (Options(..), defaultOptions)
import qualified Data.Aeson.TH as Aeson (deriveJSON)
import Data.Char (toLower)
import Language.Haskell.TH (Name, Q, Dec)

deriveJSON :: String -> Name -> Q [Dec]
deriveJSON prefix name = Aeson.deriveJSON opts name
  where
    opts = defaultOptions { fieldLabelModifier = drop $ length prefix
                          , constructorTagModifier = \(c:cs) -> toLower c : cs
                          }
