module System.Console.Jp.Options 
(Options(..), options, calculateOpts)
where

import System.FilePath ((</>))
import System.Directory (getHomeDirectory)

import System.Console.GetOpt

data Options = Options { optExpr :: Maybe String
                       , optVersion :: Bool
                       , optHelp :: Bool
                       , optModulesFile :: Maybe FilePath
                       , optColor :: Bool
                       , optPipe :: Bool
                       , optMinimize :: Bool
                       } deriving Show

defaultOptions :: Options
defaultOptions = Options { optExpr = Nothing,
                           optVersion = False,
                           optHelp = False,
                           optModulesFile = Nothing,
                           optColor = True,
                           optPipe = False,
                           optMinimize = False
                         }

options :: [OptDescr (Options -> Options)]
options = [ Option ['p'] ["pipe"] (NoArg pipeOpt) pipeDesc
          , Option ['e'] ["expr"] (ReqArg exprOpt "<expr>") exprDesc
          , Option ['m'] ["minimize"] (NoArg minimizeOpt) minimizeDesc]
    where pipeDesc = "Reads input from STDIN and outputs to STDOUT"
          pipeOpt o = o { optPipe = True }
          exprDesc = "A lens expression used to transform the input"
          exprOpt expr o = o { optExpr = Just expr }
          minimizeDesc = "Minimizes the output."
          minimizeOpt o = o { optMinimize = True }


compileOpts :: [String] -> Either [String] (Options, [String])
compileOpts argv =
    case getOpt Permute options argv of
         (o,n,[]) -> return (foldl (flip id) defaultOptions o, n)
         (_,_,errs) -> Left errs

-- | Compute the command-line options and a path for the modules file
calculateOpts :: [String] -> IO (Either [String] (Options, [String]))
calculateOpts argv = do
  homeDir <- getHomeDirectory
  case compileOpts argv of
    Left errs -> return $ Left errs
    Right (o,n) -> return $ Right (o {
      optModulesFile = Just $ homeDir
                       </> ".config"
                       </> "jp"
                       </> "modules"
      }, n)

