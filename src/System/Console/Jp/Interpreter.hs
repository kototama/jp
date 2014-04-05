module System.Console.Jp.Interpreter
(runAesonLensInterpreter)
where

import Control.Monad
import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Extension
import qualified Data.Text.Lazy as TL

import qualified Data.Aeson as Aeson

import Data.Maybe (fromJust)

runAesonLensInterpreter :: String -> String -> IO (Either String Aeson.Value)
runAesonLensInterpreter input expr = do r <- runInterpreter $ aesonLensInterpreter input expr
                                        case r of
                                          Right x -> return $ Right x
                                          Left (WontCompile errors)  -> 
                                              return $ Left ("Error in expression:\n\n" ++ errorMsgs)
                                                  where errorMsgs = unwords $ map errMsg errors
                                          Left (UnknownError msg) -> return $ Left ("Unknown error:\n\n" ++ msg)
                                          Left (NotAllowed msg) -> return $ Left ("Not allowed:\n\n" ++ msg)
                                          Left (GhcException msg) -> return $ Left ("GhcException:\n\n" ++ msg)



aesonLensInterpreter :: String -> String -> Interpreter Aeson.Value
aesonLensInterpreter input expr = do
      setImportsQ [("Prelude", Nothing), ("Data.Map", Just "M"), ("Control.Lens", Nothing), ("Data.Aeson.Lens", Nothing), ("Data.Aeson", Nothing)]
      set [languageExtensions := [OverloadedStrings]]

      let interpExpr = "(" ++ (show input) ++ " :: String)" ++ expr
      a <- interpret interpExpr (as :: Maybe Aeson.Value)
      return $ fromJust a

testHint :: Interpreter ()
testHint = do

  setImportsQ [("Prelude", Nothing), ("Data.Map", Just "M"), ("Control.Lens", Nothing), ("Data.Aeson.Lens", Nothing), ("Data.Aeson", Nothing)]
  set [languageExtensions := [OverloadedStrings]]

  let userExpr = "[{\"someObject\" : { \"version\" : [1, 42, 0] }}]"
  let expr3 = "(" ++ (show userExpr) ++ " :: String)" ++ " ^? nth 0 . key \"someObject\" . key \"version\" . nth 1"
  a <- interpret expr3 (as :: Maybe Aeson.Value)
  case fromJust a of
    Aeson.Number x -> say $ show x
    _ -> say "Error"

  -- say "We can also evaluate an expression; the result will be a string"
  -- let expr2 = "length $ concat [[1,2],[3]]"
  -- say $ concat ["e.g. eval ", show expr2]
  -- a <- eval expr2
  -- say (show a)

say :: String -> Interpreter ()
say = liftIO . putStrLn

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError e = putStrLn $ "Ups... " ++ (show e)
