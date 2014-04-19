module WatchFile (watchFiles) where


import Control.Concurrent (threadDelay)
import System.Directory (getModificationTime)
import System.IO (hFlush, stdout)


watchFiles :: IO [FilePath] -> IO a
watchFiles action = do
  files <- performAction
  lastModifiedTimes <- mapM getModificationTime files
  watchFiles' files lastModifiedTimes
    where
      watchFiles' files times = do
        currentTimes <- mapM getModificationTime files
        if (all id . zipWith (==) times $ currentTimes) then
            sleep 1000 >> watchFiles' files currentTimes
        else do
             newFiles <- performAction
             sleep 1000
             watchFiles' newFiles currentTimes
      performAction = do
        putStrLn "Processing"
        hFlush stdout
        files <- action
        putStrLn "Done."
        putStrLn . replicate 60 $ '-'
        hFlush stdout
        return files


sleep :: Int -> IO ()
sleep = threadDelay . (*1000)
