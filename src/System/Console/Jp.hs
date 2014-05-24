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

import System.Console.Jp.Options

getUsage :: IO String
getUsage = do
    pn <- getProgName
    return $ usageInfo ("Usage: " ++ pn ++ " [<options>] [<file>]") options

data JsonInput = JsonInput String Value

processJSON :: Options -> JsonInput -> IO ()
processJSON Options{ optColor = True, optExpr = Just expr, optMinimize = False} (JsonInput s _) = do
  res <- runAesonLensInterpreter s expr
  case res of
     Right v -> putDoc (encodePretty v)
     Left errMsg -> do
       putStr $ errMsg
       exitFailure

processJSON Options{ optExpr = Just expr, optMinimize = True} (JsonInput s _) = do
  res <- runAesonLensInterpreter s expr
  case res of
     Right v -> putJsonMinimized v
     Left errMsg -> do
       putStr $ errMsg
       exitFailure

processJSON Options{ optColor = True, optExpr = Nothing, optMinimize = False} (JsonInput _ v) = do
  putDoc $ encodePretty v

processJSON Options{ optColor = True, optExpr = Nothing, optMinimize = True} (JsonInput _ v) = do
  putJsonMinimized v

processJSON _ _ = do
  getUsage >>= putStr
  exitSuccess

putJsonMinimized :: Value -> IO ()
putJsonMinimized v = putStr . (flip displayS "") $ doc
    where doc = renderCompact (encodePretty' compactConfig v)

processInput :: Options -> String -> IO ()
processInput opts input =
  case eitherDecode (C.pack input) of
    Left errMsg -> do
      putStr $ errMsg
      exitFailure
    Right v ->
      processJSON opts (JsonInput input v)


main :: IO ()
main = do
  args <- getArgs
  o <- calculateOpts args
  case o of
    Left _ -> do
      getUsage >>= putStr
      exitFailure
    Right (Options {optPipe = False}, []) -> do
      getUsage >>= putStr
      exitSuccess
    Right (opts@Options {optPipe = True}, _) -> do
      input <- getContents
      processInput opts input
    Right (opts, files) -> do
      -- TODO: better repeat the process instead of concat
      input <- fmap concat . mapM readFile $ files
      processInput opts input
