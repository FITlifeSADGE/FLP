import System.IO
import System.Environment
import Data.List.Split
import Data.List
import Debug.Trace (trace)
import Data.Maybe (catMaybes)

type NodeData = (String, Int, String, Int)

-- Čte text ze souboru
loadFile :: FilePath -> IO String
loadFile filePath = do
    contents <- readFile filePath
    return contents

rozdelText :: String -> [String]
rozdelText text = splitOn "\n" text

pridejUroven :: [String] -> [(String, Int)]
pridejUroven a = map (\a -> (a, length (takeWhile (==' ') a) `div` 2)) a

transformList :: [(String, Int)] -> [NodeData]
transformList = map parseElement

parseElement :: (String, Int) -> NodeData
parseElement (s, lvl) =
  let (prefix:rest) = words s
      (nodeType, restString) = case prefix of
        "Node:" -> ("Node", unwords rest)
        "Leaf:" -> ("Leaf", unwords rest)
        _ -> error "Unrecognized pattern"
      splitRest = splitOn ", " restString
      number = if nodeType == "Node" then read (head splitRest) :: Int else -1 -- Leaf nemá index příznaku
      value = if nodeType == "Node" then splitRest !! 1 else head splitRest
  in (nodeType, number, value, lvl)

findLeaf :: [NodeData] -> [Double] -> String
findLeaf nodes numbers = findLeafHelper nodes numbers 0 0 where
  findLeafHelper :: [NodeData] -> [Double] -> Int -> Int -> String
  findLeafHelper [] _ _ _ = error "No leaf found"
  findLeafHelper _ [] _ _ = error "No leaf found"
  findLeafHelper nodes@((typ,index,value,level):xs) numbers skips levelToFind
    | typ == "Node" && comparisonNumber >= read value && skips == 0 && level == levelToFind = 
        findLeafHelper xs numbers 1 (level + 1)
    | typ == "Node" && comparisonNumber < read value && skips == 0 && level == levelToFind = 
        findLeafHelper xs numbers 0 (level + 1)
    | typ == "Leaf" && skips > 0 && level >= levelToFind = 
        findLeafHelper xs numbers (skips - 1) levelToFind
    | typ == "Leaf" && skips == 0 && level == levelToFind = 
        value
    | otherwise = findLeafHelper xs numbers skips levelToFind
    where comparisonNumber = numbers !! index

convertToDoubleLists :: String -> [[Double]]
convertToDoubleLists contents = do
  doubleList <- map (map read . splitOn ",") $ lines contents
  return doubleList

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("-1":treeFile:newDataFile:_) -> do -- První podúkol
            treeContents <- loadFile treeFile
            newDataContents <- loadFile newDataFile
            let transformedList = transformList (pridejUroven (rozdelText treeContents))
            let doubleList = convertToDoubleLists newDataContents
            let result = map (findLeaf transformedList) doubleList
            mapM_ putStrLn result
        ("-2":trainingDataFile:_) -> do -- Druhý podúkol
            trainingDataContents <- loadFile trainingDataFile
            putStrLn "Data ze souboru obsahujici trenovaci data:"
            putStrLn trainingDataContents
        _ -> putStrLn "Nespravne argumenty"
