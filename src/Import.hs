module Import where


import Data.IORef
import Text.Regex.Posix ((=~))


type Modules = [(FilePath, String)]


importModules :: FilePath -> IO Modules
importModules fileName = do
  r <- newIORef []
  importModules' r fileName

importModules' :: IORef [FilePath] -> FilePath -> IO Modules
importModules' r fileName = do
  s <- readFile fileName
  modifyIORef r (fileName:)
  visited <- readIORef r
  modifyIORef r (modules s ++)
  unvisited <- return . filter (not . (`elem`visited)) $ modules s
  moduleNames <- mapM (importModules' r) $ unvisited
  return $ (fileName, s) : (concat moduleNames)
    where
      modules :: String -> [FilePath]
      modules s = map (!!1) $ s =~ "@import[ ]*[(][ ]*([^)]+)[)]"

performImport :: FilePath -> IO String
performImport s = return . concat . reverse . map snd =<< importModules s

concatModules :: Modules -> IO String
concatModules = return . concat . reverse . map snd

listupModules :: Modules -> IO [FilePath]
listupModules = return . map fst
