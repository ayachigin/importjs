module Options ( Options(..)
               , getOptions) where


import System.Console.GetOpt
import System.Environment (getProgName)
import System.Exit (exitFailure)


data Options = Options
    { optOutput      :: FilePath
    , optWatch       :: Bool
    } deriving Show

defaultOptions :: Options
defaultOptions    = Options
                    { optOutput = "out"
                    , optWatch  = False
                    }

options :: [OptDescr (Options -> Options)]
options =
 [ Option ['o']     ["output"]
     (ReqArg (\ f opts -> opts { optOutput = f})
             "FILE")
     "output FILE"
 , Option ['w']     []
     (NoArg ((\opts -> opts { optWatch = True })))
     "watching FILE"
 ]

getOptions :: String -> [String] -> IO (Options, [String])
getOptions progName argv =
   case getOpt Permute options argv of
     (_, [], _) -> exitFailure'
     (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
     (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where
    exitFailure' = do
       putStrLn $ usageInfo header options
       exitFailure
    header = "Usage: " ++ progName ++ " [OPTION...] files..."
