{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module System.Console.Jp
(
 main
) where

import System.Environment (getArgs, getProgName)
import System.Console.GetOpt (usageInfo)
import System.Exit (exitFailure,exitSuccess)

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

import Text.PrettyPrint.ANSI.Leijen
import System.Console.Jp.Pretty

import Data.Monoid (mconcat)
import Control.Monad (join)

import Data.Aeson

import System.Console.Jp.Interpreter

import System.Console.Jp.Options

v1 :: Maybe Value
v1 = decode "{\"foo\":1,\"bar\":\"bar\", \"z\": null, \"zz\": true, \"emb\": {\"foo\":1,\"bar\":\"bar\", \"z\": null, \"zz\": true}}"

-- v2 :: Maybe [Int]
-- v2 = decode "[1,2,3,4,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,5]"

v3 :: Maybe Value
v3 = decode "[1,2,3,4,54, {\"foo\":1,\"bar\":\"bar\", \"z\": null, \"zz\": true}]"

-- someText :: [Doc]
-- someText = map text ["words","in","a","tuple"]

getUsage :: IO String
getUsage = do
    pn <- getProgName
    return $ usageInfo ("Usage: " ++ pn ++ " [<options>] [<file>]") options

data JsonInput = JsonInput String Value

processJSON :: Options -> JsonInput -> IO ()
processJSON Options{ optCompact = False, optColor = True, optExpr = Just expr} (JsonInput s _) = do
  res <- runAesonLensInterpreter s (C.unpack expr)
  case res of
     Right v -> putDoc (encodePretty v)
     Left errMsg -> do
       putStr $ errMsg
       exitFailure

processJSON Options{ optCompact = False, optColor = True, optExpr = Nothing} (JsonInput _ v) = do
  putDoc $ encodePretty v

processJSON _ _ = do
  getUsage >>= putStr
  exitSuccess

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
  -- str <- B.getContents
  -- putDoc $ encodePretty (fromJust $ (decode str) :: Maybe Value)
  -- runJpInterpreter
  args <- getArgs
  case compileOpts args of
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
--      putStr $ "files" ++ (show files) ++ " options " ++ (show opts)
      
  -- input <- getLine
  -- res <- runAesonLensInterpreter input (head args)
  -- case res of
  --   Right v -> putDoc (encodePretty v)
  --   Left errMsg -> putStr $ errMsg

  
  -- putStr "\n"
  -- putDoc $ encodePretty (fromJust v1)

--  putDoc $ (dullgreen (encodePretty (fromJust v1)))
  -- args <- getArgs
  -- case getOpt Permute options args of
  --   (o,n,[] ) -> do
  --        jp o n
  --        return ()
  --   (_,_,errs) -> putStr $ show errs -- ioError (userError "blabla")

