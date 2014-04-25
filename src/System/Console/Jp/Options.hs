
module System.Console.Jp.Options 
(compileOpts)
where

import qualified Data.ByteString.Lazy as B
import qualified System.FilePath as FP

import System.Console.GetOpt

data Options = Options { optExpr :: Maybe B.ByteString
                       , optVersion :: Bool
                       , optHelp :: Bool
                       , optModuleFile :: Maybe FP.FilePath
                       , optPrettyPrint :: Bool
                       } deriving Show

defaultOptions :: Options
defaultOptions = Options { optExpr = Nothing
                         , optVersion = False
                         , optHelp = False
                         , optModuleFile = Just "/tmp/"
                         , optPrettyPrint = True
                         }

options :: [OptDescr (Options -> Options)]
options = [Option ['p'] ["pretty-print"] (NoArg prettyPrintOpt) prettyPrintDesc]  
    where prettyPrintDesc = "Output the data without any transformations. Can be used to pretty-print JSON data."
          prettyPrintOpt o = o { optPrettyPrint = True }


compileOpts :: [String] -> IO (Options, [String])
compileOpts argv =
    case getOpt Permute options argv of
         (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
         (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
     where header = "Usage: ic [OPTION...] files..."
