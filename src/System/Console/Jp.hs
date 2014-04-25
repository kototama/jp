{-# LANGUAGE OverloadedStrings #-}

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

import Data.Maybe (fromJust)

import Data.Aeson

import System.Console.Jp.Interpreter

import System.Console.Jp.Options

-- getUsage :: IO String
-- getUsage = do
--   pn <- getProgName
--   return $ usageInfo ("Usage: " ++ pn ++ "[<option>]") options

-- compilerOpts :: [String] -> IO

-- jp :: [Options] -> [String] -> IO ()
-- jp opts nonOpts = do
--   putStr "here"
--   putStr $ show opts
--   mapM_ putStr nonOpts

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

main :: IO ()
main = do
  -- str <- B.getContents
  -- putDoc $ encodePretty (fromJust $ (decode str) :: Maybe Value)
  -- runJpInterpreter
  args <- getArgs
  case compileOpts args of
    Left errs ->
      mapM_ putStr errs
    Right (Options {optPipe = False}, []) -> do
      getUsage >>= putStr
      exitSuccess
    Right (opts, files) ->
      putStr $ "files" ++ (show files) ++ " options " ++ (show opts)
      
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

