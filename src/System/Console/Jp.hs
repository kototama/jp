{-# LANGUAGE OverloadedStrings #-}

module System.Console.Jp 
(
 main
) where

import System.Environment (getArgs,getProgName)
import System.Console.GetOpt

import System.Console.ANSI
import Text.PrettyPrint.ANSI.Leijen

import System.Console.Jp.Options
import System.Console.Jp.Pretty

import qualified Data.ByteString.Lazy as B

import Data.Maybe (fromJust)

import Data.Aeson


getUsage :: IO String
getUsage = do
  pn <- getProgName
  return $ usageInfo ("Usage: " ++ pn ++ "[<option>]") options

-- compilerOpts :: [String] -> IO 

jp :: [Options] -> [String] -> IO ()
jp opts nonOpts = do
  putStr "here"
  putStr $ show opts
  mapM_ putStr nonOpts

v1 = decode "{\"foo\":1,\"bar\":\"bar\"}" :: Maybe Value
v2 = decode "[1,2,3,4,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,5]" :: Maybe [Int]
v3 = decode "[1,2,3,4,54]" :: Maybe [Int]
someText = map text ["words","in","a","tuple"]

main :: IO ()
main = do
  putDoc $ red (encodePretty (fromJust v3))
--  putDoc $ nest 2 (text "hello" <$> text "world" <$> "third") <$> text "!"
--  putDoc $ parens (align (vcat (punctuate (space <> comma) someText)))


--  putDoc $ (dullgreen (encodePretty (fromJust v1)))
  -- args <- getArgs
  -- case getOpt Permute options args of
  --   (o,n,[] ) -> do
  --        jp o n
  --        return ()
  --   (_,_,errs) -> putStr $ show errs -- ioError (userError "blabla")

