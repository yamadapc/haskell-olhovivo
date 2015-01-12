{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Web.OlhoVivo where

import Control.Lens ((^.), (^?))
import Data.Aeson (FromJSON)
import Data.Aeson.TH (Options(..), deriveJSON, defaultOptions)
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.Default (Default(..))
import Data.Text (Text, pack)
import Network.Wreq (asJSON, defaults, responseBody)
import Network.Wreq.Types (Options(..))
import Network.Wreq.Session

data OlhoVivoApiOptions = OlhoVivoApiOptions { olhovivoApiVersion :: String
                                             , olhovivoApiBaseUrl :: String
                                             }
  deriving(Eq, Ord, Show)

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
  deriving(Eq, Ord, Show)

$(deriveJSON
    defaultOptions { fieldLabelModifier = drop $ length
                                              ("olhovivoLine" :: String)
                   , constructorTagModifier = \(c:cs) -> toLower c : cs
                   }
    ''OlhoVivoLine
 )

data OlhoVivoStop =
    OlhoVivoStop { olhovivoStopCodigoParada :: Int
                 , olhovivoStopNome :: String
                 , olhovivoStopEndereco :: String
                 , olhovivoStopLatitude :: Double
                 , olhovivoStopLongitude :: Double
                 }
  deriving(Eq, Ord, Show)

$(deriveJSON
    defaultOptions { fieldLabelModifier = drop $ length
                                              ("olhovivoStop" :: String)
                   , constructorTagModifier = \(c:cs) -> toLower c : cs
                   }
    ''OlhoVivoStop
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

olhoVivoGet :: FromJSON a => Session -> OlhoVivoApiOptions -> String
            -> Network.Wreq.Types.Options -> IO [a]
olhoVivoGet session opts endpoint reqOpts = do
    let url = urlForEndpoint opts endpoint
    json <- asJSON =<< getWith reqOpts session url
    return $ json ^. responseBody

olhoVivoLines :: Session -> OlhoVivoApiOptions -> Text -> IO [OlhoVivoLine]
olhoVivoLines session opts q =
    let reqOpts = defaults { params = [ ("termosBusca", q) ]
                           }
      in olhoVivoGet session opts "/Linha/Buscar" reqOpts

olhoVivoStops :: Session -> OlhoVivoApiOptions -> Text -> IO [OlhoVivoStop]
olhoVivoStops session opts q =
    let reqOpts = defaults { params = [ ("termosBusca", q) ]
                           }
      in olhoVivoGet session opts "/Parada/Buscar" reqOpts

olhoVivoStopsInLine :: Session -> OlhoVivoApiOptions -> Int -> IO [OlhoVivoStop]
olhoVivoStopsInLine session opts lineCode =
    let reqOpts = defaults { params = [ ("codigoLinha", pack $ show lineCode) ]
                           }
      in olhoVivoGet session opts "/Parada/BuscarParadasPorLinha" reqOpts

olhoVivoStopsInExpressLane :: Session -> OlhoVivoApiOptions -> Int
                           -> IO [OlhoVivoStop]
olhoVivoStopsInExpressLane session opts expressLaneCode =
    let reqOpts = defaults { params =
                                 [ ("codigoLinha" , pack $ show expressLaneCode)
                                 ]
                           }
      in olhoVivoGet session opts "/Parada/BuscarParadasPorLinha" reqOpts

urlForEndpoint :: OlhoVivoApiOptions -> String -> String
urlForEndpoint opts endpoint =
    olhovivoApiBaseUrl opts ++ olhovivoApiVersion opts ++ endpoint
