
module System.Console.Jp.Options 
(Options(..), options, compileOpts)
where

import qualified Data.ByteString.Lazy as B
import qualified System.FilePath as FP

import System.Console.GetOpt

data Options = Options { optExpr :: Maybe B.ByteString
                       , optVersion :: Bool
                       , optHelp :: Bool
                       , optModuleFile :: Maybe FP.FilePath
                       , optCompact :: Bool
                       , optColor :: Bool
                       , optPipe :: Bool
                       } deriving Show

defaultOptions :: Options
defaultOptions = Options { optExpr = Nothing,
                           optVersion = False,
                           optHelp = False,
                           optModuleFile = Just "/tmp/",
                           optCompact = False,
                           optColor = True,
                           optPipe = False
                         }

options :: [OptDescr (Options -> Options)]
options = [Option ['p'] ["pipe"] (NoArg pipeOpt) pipeDesc]  
    where pipeDesc = "Reads input from STDIN and outputs to STDOUT"
          pipeOpt o = o { optPipe = True }


compileOpts :: [String] -> Either [String] (Options, [String])
compileOpts argv =
    case getOpt Permute options argv of
         (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
         (_,_,errs) -> Left errs
