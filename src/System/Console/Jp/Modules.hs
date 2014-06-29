module System.Console.Jp.Modules (readModules) where

import System.FilePath ()
import Control.Monad (liftM)
import Text.Read (readEither)

type Module = (String, Maybe String)

readModules :: FilePath -> IO (Either String [Module])
readModules path = liftM readEither (readFile path)
