module System.Console.Jp.Modules (readModules,
                                  defaultModules,
                                  Module)
       where
import System.FilePath ()
import Control.Monad (liftM)
import Text.Read (readEither)

type Module = (String, Maybe String)

defaultModules :: [Module]
defaultModules = [("Prelude", Nothing), 
                   ("Data.Map", Just "M"), 
                   ("Control.Lens", Nothing), 
                   ("Data.Aeson.Lens", Nothing), 
                   ("Data.Aeson", Nothing)]

-- | Read and parse the modules file
readModules :: FilePath -> IO (Either String [Module])
readModules path = liftM readEither (readFile path)
