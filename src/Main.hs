module Main where


import System.Environment (getArgs, getProgName)
import Control.Monad (when)
import System.Exit (exitSuccess, exitFailure)


import Import (importModules, concatModules, listupModules)
import Options (getOptions, Options(..))
import WatchFile (watchFiles)


main :: IO [FilePath]
main = do
  args <- getArgs
  progName <- getProgName
  (options, (input:_)) <- getOptions progName args
  let out = optOutput options
  if optWatch options then do
      watchFiles $ performImport input out
  else
      performImport input out

performImport :: FilePath -> FilePath -> IO [FilePath]
performImport filename out = do
  modules <- importModules filename
  writeFile out =<< concatModules modules
  listupModules modules
