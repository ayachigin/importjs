module Parser where


import Text.Regex.Posix ((=~))


moduleNames :: String -> IO [(String, String)]
moduleNames fileName = do
  s <- readFile fileName
  modules <- mapM moduleNames $ moduleNames' s
  return $ (fileName, s) : (concat modules)
    where
      moduleNames' :: String -> [String]
      moduleNames' s = map (!!1) $ s =~ "@import[ ]*[(][ ]*([^)]+)[)]"
