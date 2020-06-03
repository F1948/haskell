module IO where
import System.IO
import System.Directory
--stepic 5.5

-- функция поиска подстроки в строке
-- isCont "Hello" "ll" -> True
isCont :: String -> String -> Bool
isCont string key = isCont' string key (length key) where
  isCont' [] key _ = key == []
  isCont' string key l = take l string == key || isCont' (tail string) key l

main' :: IO ()
main' = do
    putStr "Substring: "
    s1 <- getLine
    if s1 == "" then do putStrLn "Canceled"
    else do
        dirContent <- getDirectoryContents "." -- [FilePath]
        fileList <- return $ (filter (`isCont` s1)) dirContent
        messageList <- return $ map ("Removing file: " ++ ) fileList
        mapM_ putStrLn messageList
        mapM_ removeFile fileList
        return ()
