{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module System.Console.Jp
(
 main
) where

import System.Environment (getArgs, getProgName)
import System.Console.GetOpt (usageInfo)
import System.Exit (exitFailure,exitSuccess)
import qualified Data.ByteString.Lazy.Char8 as C
import Text.PrettyPrint.ANSI.Leijen
import System.Console.Jp.Pretty
import Data.Aeson
import System.Console.Jp.Interpreter
import System.Console.Jp.Options as O
import qualified System.Console.Jp.Modules as M

data St = St { options :: Options,
               modules :: [M.Module],
               files :: [FilePath]
             }

getUsage :: IO String
getUsage = do
    pn <- getProgName
    return $ usageInfo ("Usage: " ++ pn ++ " [<options>] [<file>]") O.options

data JsonInput = JsonInput String Value

processJSON :: St -> JsonInput -> IO ()
processJSON St { options = Options{ optColor = True,
                                    optExpr = Just expr,
                                    optMinimize = False}}
  (JsonInput s _) = do
  res <- runAesonLensInterpreter s expr
  case res of
     Right v -> putDoc (encodePretty v)
     Left errMsg -> do
       putStr $ errMsg
       exitFailure

processJSON St { options = Options{ optExpr = Just expr, optMinimize = True}}
  (JsonInput s _) = do
  res <- runAesonLensInterpreter s expr
  case res of
     Right v -> putJsonMinimized v
     Left errMsg -> do
       putStr $ errMsg
       exitFailure

processJSON St { options = Options{ optColor = True, optExpr = Nothing, optMinimize = False}}
  (JsonInput _ v) = do
  putDoc $ encodePretty v

processJSON St { options = Options{ optColor = True, optExpr = Nothing, optMinimize = True}}
  (JsonInput _ v) = do
  putJsonMinimized v

processJSON _ _ = do
  getUsage >>= putStr
  exitSuccess

putJsonMinimized :: Value -> IO ()
putJsonMinimized v = putStr . (flip displayS "") $ doc
    where doc = renderCompact (encodePretty' compactConfig v)

processInput :: St -> String -> IO ()
processInput st input =
  case eitherDecode (C.pack input) of
    Left errMsg -> do
      putStr $ errMsg
      exitFailure
    Right v ->
      processJSON st (JsonInput input v)

getModules :: Maybe FilePath -> IO (Either String [M.Module])
getModules Nothing = return $ Right M.defaultModules
getModules (Just path) = M.readModules path

calculateState :: [String] -> IO St
calculateState args = do
  o <- calculateOpts args
  case o of
    Left _ -> do
      getUsage >>= putStr
      exitSuccess
    Right (ops, fls) -> do
      x <- getModules $ optModulesFile ops
      case x of
        Left msg -> do
          putStr msg
          exitSuccess
        Right md ->
          return St { options = ops,
                      modules = md,
                      files = fls }

main :: IO ()
main = do
  args <- getArgs
  st <- calculateState args
  case st of
    St { options = Options {optPipe = False}, files = []} -> do
      getUsage >>= putStr
      exitSuccess
    St { options = Options {optPipe = True}, files = []} -> do
      input <- getContents
      processInput st input
    St { files = fls} -> do
      input <- fmap concat . mapM readFile $ fls
      processInput st input
