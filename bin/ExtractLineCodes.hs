import Control.Applicative ((<$>))
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (filterM, unless, when)
import qualified Data.Csv as Csv
import qualified Data.ByteString.Lazy as ByteString.Lazy (readFile)
import Data.Default (def)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import qualified Data.Vector as Vector (Vector, foldl)
import Network.Wreq.Session (Session, withSession)
import System.Environment (getArgs, getEnv)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, hPrint, stderr)
import Text.Read (readMaybe)
import Web.OlhoVivo (LineCode, OlhoVivoLine(..), newOlhoVivoApi, olhoVivoLines)

main :: IO ()
main = do
    args <- getArgs

    withSession $ \session -> do
        when (length args < 2) $ do
            hPutStrLn stderr usageString
            exitWith (ExitFailure 1)

        token <- getEnv "SPTRANS_TOKEN"
        authenticated <- newOlhoVivoApi session def (fromString token)

        unless authenticated $ do
            hPutStrLn stderr "Failed to authenticate with the Olho Vivo API"
            exitWith (ExitFailure 3)

        let inputFilePath:outputFilePath:_ = args
        eentries <- Csv.decode Csv.HasHeader <$>
                    ByteString.Lazy.readFile inputFilePath

        case eentries of
            Left err -> do
                hPrint stderr err
                exitWith (ExitFailure 2)
            Right entries -> do
                lineCodes <- concatMapConcurrently
                                 (lineCodeFromName session)
                                 (getLineNames entries)

                let output = foldr (\l m -> show l ++ "," ++ m) "" (nub lineCodes)
                writeFile outputFilePath output

concatMapConcurrently :: (a -> IO [b]) -> [a] -> IO [b]
concatMapConcurrently f xs = do
    ys <- mapConcurrently f xs
    return $ concat ys

usageString :: String
usageString = "Usage: olhovivo-extract-line-codes <input-file> <output-file>"

getLineNames :: Vector.Vector [String] -> [String]
getLineNames = Vector.foldl helper []
  where
    helper m el = takeWhile (/= '-') (el !! 3) : m

lineCodeFromName :: Session -> String -> IO [LineCode]
lineCodeFromName session lineName = do
    putStrLn $ "Validating " ++ lineName ++ "..."

    lines <- olhoVivoLines session def (fromString lineName)
    putStrLn $
        "GET /Linha/Buscar " ++ show lineName ++ " " ++
        show (map olhovivoLineCodigoLinha lines)

    let valid = not (null lines)
    putStrLn $ "Validated " ++ lineName ++ ": " ++ show valid
    return $ map olhovivoLineCodigoLinha lines
