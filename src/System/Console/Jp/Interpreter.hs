module System.Console.Jp.Interpreter
(runAesonLensInterpreter)
where

import Language.Haskell.Interpreter
import qualified System.Console.Jp.Modules as M

import Data.Aeson

runAesonLensInterpreter :: String -> String -> [M.Module] -> IO (Either String Value)
runAesonLensInterpreter input expr modules = do
  r <- runInterpreter $ aesonLensInterpreter input expr modules
  case r of
    Right (Just x) -> return $ Right x
    Right Nothing -> return $ Left "Nothing matched"
    Left (WontCompile errors)  -> 
      return $ Left ("Error in expression:\n\n" ++ errorMsgs)
      where errorMsgs = unwords $ map errMsg errors
    Left (UnknownError msg) -> return $ Left ("Unknown error:\n\n" ++ msg)
    Left (NotAllowed msg) -> return $ Left ("Not allowed:\n\n" ++ msg)
    Left (GhcException msg) -> return $ Left ("GhcException:\n\n" ++ msg)



aesonLensInterpreter :: String -> String -> [M.Module] -> Interpreter (Maybe Value)
aesonLensInterpreter input expr modules = do
      setImportsQ modules
      set [languageExtensions := [OverloadedStrings]]

      let interpExpr = "toJSON $ (" ++ (show input) ++ " :: String) ^? _Value" ++ expr
      v <- interpret interpExpr (as :: Value)
      return $ Just v
