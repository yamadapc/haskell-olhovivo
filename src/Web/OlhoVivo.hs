{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.OlhoVivo where

import Control.Lens ((^.), (^?))
import Data.Aeson.TH (Options(..), deriveJSON, defaultOptions)
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Default (Default(..))
import Data.Text (Text)
import Network.Wreq (asJSON, defaults, responseBody)
import Network.Wreq.Types (Options(..))
import Network.Wreq.Session

data OlhoVivoApiOptions = OlhoVivoApiOptions { olhovivoApiVersion :: String
                                             , olhovivoApiBaseUrl :: String
                                             }

data OlhoVivoLine =
    OlhoVivoLineSummary { olhovivoLineCodigoLinha :: Int
                        , olhovivoLineCircular :: Bool
                        , olhovivoLineLetreiro :: String
                        , olhovivoLineSentido :: Int
                        , olhovivoLineTipo :: Int
                        , olhovivoLineDenominacaoTPTS :: String
                        , olhovivoLineDenominacaoTSTP :: String
                        , olhovivoLineInformacoes :: Maybe String
                        }
  deriving(Show)

$(deriveJSON
    defaultOptions { fieldLabelModifier = drop $ length
                                              ("olhovivoLine" :: String)
                   , constructorTagModifier = \(c:cs) -> toLower c : cs
                   }
    ''OlhoVivoLine
 )

instance Default OlhoVivoApiOptions where
  def = OlhoVivoApiOptions
            { olhovivoApiBaseUrl = "http://api.olhovivo.sptrans.com.br/v"
            , olhovivoApiVersion = "0"
            }

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
       Nothing -> return False

olhoVivoLinhas :: Session -> OlhoVivoApiOptions -> Text -> IO [OlhoVivoLine]
olhoVivoLinhas session opts q = do
    let url = urlForEndpoint opts "/Linha/Buscar"
        reqOpts = defaults { params = [ ("termosBusca", q) ]
                           }

    json <- asJSON =<< getWith reqOpts session url
    return $ json ^. responseBody


urlForEndpoint :: OlhoVivoApiOptions -> String -> String
urlForEndpoint opts endpoint =
    olhovivoApiBaseUrl opts ++ olhovivoApiVersion opts ++ endpoint
