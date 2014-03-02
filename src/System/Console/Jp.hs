module System.Console.Jp (
main
) where

import System.Environment (getArgs,getProgName)
import System.Console.GetOpt
import System.Console.Jp.Options

import System.Console.ANSI
import Text.PrettyPrint.ANSI.Leijen

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

main :: IO ()
main = do
  setTitle "ANSI Terminal Short Example"

  setSGR [ SetConsoleIntensity BoldIntensity
         , SetColor Foreground Vivid Red
         ]
  putStr "Hello"

  setSGR [ SetConsoleIntensity NormalIntensity
         , SetColor Foreground Vivid White
         , SetColor Background Dull Blue
         ]
  putDoc $ red (text "Red") <> comma <+> white (text "white") <+> text "and"
                <+> blue (text "blue") <> char '!' <> linebreak
  putDoc $ blue (text "Nested" <+> dullyellow (text "colors") <+> text "example")
                <> linebreak
  args <- getArgs
  case getOpt Permute options args of
    (o,n,[] ) -> do
         jp o n
         return ()
    (_,_,errs) -> putStr $ show errs -- ioError (userError "blabla")

