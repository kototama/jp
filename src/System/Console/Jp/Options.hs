

module System.Console.Jp.Options 
(options, Options)
where

import System.Console.GetOpt

data Options = PrintOpt deriving Show

options :: [OptDescr Options]
options = [Option ['p'] ["print"] (NoArg PrintOpt) printDesc]  
        where printDesc = "Output the data without any transformations. Can be used to pretty-print JSON data."

