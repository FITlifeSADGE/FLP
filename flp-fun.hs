import System.IO
import System.Environment

-- Funkce pro načtení obsahu souboru
loadFile :: FilePath -> IO String
loadFile filePath = do
    contents <- readFile filePath
    return contents

-- Hlavní funkce
main :: IO ()
main = do
    args <- getArgs
    case args of
        ("-1":treeFile:newDataFile:_) -> do
            treeContents <- loadFile treeFile
            newDataContents <- loadFile newDataFile
            -- Zde můžete s načtenými daty pracovat dále
            putStrLn "Data ze souboru obsahujici strom:"
            putStrLn treeContents
            putStrLn "Data ze souboru obsahujici nove data:"
            putStrLn newDataContents
        ("-2":trainingDataFile:_) -> do
            trainingDataContents <- loadFile trainingDataFile
            -- Zde můžete s načtenými daty pracovat dále
            putStrLn "Data ze souboru obsahujici trenovaci data:"
            putStrLn trainingDataContents
        _ -> putStrLn "Nespravne argumenty"
