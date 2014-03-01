module System.Console.Jp (
main
) where

import System.Environment (getArgs,getProgName)
import System.Console.GetOpt (usageInfo)
import System.Console.Jp.Options

getUsage :: IO String
getUsage = do
         pn <- getProgName
         return $ usageInfo ("Usage: " ++ pn ++ "[<option>]") options

main :: IO ()
main = do
     usage <- getUsage
     putStr usage
