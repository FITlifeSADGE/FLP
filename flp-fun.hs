import System.IO
import System.Environment
import Data.List.Split
import Data.List
import Debug.Trace (trace)

type NodeData = (String, String, Int)

transformList :: [(String, Int)] -> [NodeData]
transformList = map parseElement

parseElement :: (String, Int) -> NodeData
parseElement (s, lvl) = case words s of
    ("Node:":n:value:_) -> ("Node", value, lvl) -- Hodnota je nyní řetězcem
    ("Leaf:":cls) -> ("Leaf", unwords cls, lvl)
    _ -> error "Unrecognized pattern"

-- findLeaf :: [NodeData] -> [Double] -> Maybe String
-- findLeaf nodes numbers = findLeafHelper nodes numbers 0 where
--   findLeafHelper :: [NodeData] -> [Double] -> Int -> Maybe String
--   findLeafHelper [] _ _ = Nothing
--   findLeafHelper _ [] _ = Nothing
--   findLeafHelper nodes@(current@(typ,value,level):xs) (n:ns) skips
--     | typ == "Node" && read value < n && skips == 0 = 
--         trace ("Node " ++ value ++ " is smaller, looking for a second node at level " ++ show (level + 1)) $ findLeafHelper xs (n:ns) 1
--     | typ == "Node" && read value >= n && skips == 0 = 
--         trace ("Node " ++ value ++ " is not smaller, looking for leaf at level " ++ show (level + 1)) $ findLeafHelper xs ns 0
--     | typ == "Leaf" && skips > 0 = 
--         trace ("Skipping Leaf " ++ value ++ " at level " ++ show level) $ findLeafHelper xs (n:ns) (skips - 1)
--     | typ == "Leaf" && skips == 0 = 
--         trace ("Found Leaf " ++ value ++ " at level " ++ show level) $ Just value
--     | otherwise = findLeafHelper xs (n:ns) skips

findLeaf :: [NodeData] -> [Double] -> Maybe String
findLeaf nodes numbers = findLeafHelper nodes (numbers ++ [0]) 0 where
  findLeafHelper :: [NodeData] -> [Double] -> Int -> Maybe String
  findLeafHelper [] _ _ = Nothing
  findLeafHelper _ [] _ = Nothing
  findLeafHelper nodes@(current@(typ,value,level):xs) numbers@(n:ns) skips
    | typ == "Node" && read value < n && skips == 0 = 
        trace ("Node " ++ value ++ " is smaller, moving to next number and looking for a second node at level " ++ show (level + 1)) $ findLeafHelper xs ns 1
    | typ == "Node" && read value >= n && skips == 0 = 
        trace ("Node " ++ value ++ " is not smaller, moving to next number and looking for leaf at level " ++ show (level + 1)) $ findLeafHelper xs ns 0
    | typ == "Leaf" && skips > 0 = 
        trace ("Skipping Leaf " ++ value ++ " at level " ++ show level) $ findLeafHelper xs numbers (skips - 1)
    | typ == "Leaf" && skips == 0 = 
        trace ("Found Leaf " ++ value ++ " at level " ++ show level) $ Just value
    | otherwise = findLeafHelper xs numbers skips



-- Čte text ze souboru
loadFile :: FilePath -> IO String
loadFile filePath = do
    contents <- readFile filePath
    return contents

rozdelText :: String -> [String]
rozdelText text = splitOn "\n" text

pridejUroven :: [String] -> [(String, Int)]
pridejUroven a = map (\a -> (a, length (takeWhile (==' ') a) `div` 2)) a

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("-1":treeFile:newDataFile:_) -> do -- První podúkol
            treeContents <- loadFile treeFile
            newDataContents <- loadFile newDataFile
            putStrLn "Data ze souboru obsahujici strom:"
            --print (pridejUroven (rozdelText treeContents))
            print (transformList (pridejUroven (rozdelText treeContents)))
            --putStrLn "Data ze souboru obsahujici nove data:"
            --putStrLn newDataContents
            print (findLeaf (transformList (pridejUroven (rozdelText treeContents))) ([6,3.1]))
        ("-2":trainingDataFile:_) -> do -- Druhý podúkol
            trainingDataContents <- loadFile trainingDataFile
            putStrLn "Data ze souboru obsahujici trenovaci data:"
            putStrLn trainingDataContents
        _ -> putStrLn "Nespravne argumenty"
