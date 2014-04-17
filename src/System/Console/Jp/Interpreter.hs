module System.Console.Jp.Interpreter
(runAesonLensInterpreter)
where

import Control.Monad
import Language.Haskell.Interpreter
-- import Language.Haskell.Interpreter.Extension
-- import qualified Data.Text.Lazy as TL

import Data.Aeson

runAesonLensInterpreter :: String -> String -> IO (Either String Value)
runAesonLensInterpreter input expr = do r <- runInterpreter $ aesonLensInterpreter input expr
                                        case r of
                                          Right (Just x) -> return $ Right x
                                          Right Nothing -> return $ Left "Nothing matched"
                                          Left (WontCompile errors)  -> 
                                              return $ Left ("Error in expression:\n\n" ++ errorMsgs)
                                                  where errorMsgs = unwords $ map errMsg errors
                                          Left (UnknownError msg) -> return $ Left ("Unknown error:\n\n" ++ msg)
                                          Left (NotAllowed msg) -> return $ Left ("Not allowed:\n\n" ++ msg)
                                          Left (GhcException msg) -> return $ Left ("GhcException:\n\n" ++ msg)



aesonLensInterpreter :: String -> String -> Interpreter (Maybe Value)
aesonLensInterpreter input expr = do
  -- todo read this from a config file
      setImportsQ [("Prelude", Nothing), 
                   ("Data.Map", Just "M"), 
                   ("Control.Lens", Nothing), 
                   ("Data.Aeson.Lens", Nothing), 
                   ("Data.Aeson", Nothing)]
      set [languageExtensions := [OverloadedStrings]]

      let interpExpr = "toJSON $ (" ++ (show input) ++ " :: String) ^? _Value" ++ expr
      v <- interpret interpExpr (as :: Value)
      return $ Just v
