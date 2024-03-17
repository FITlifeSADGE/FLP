import System.IO
import System.Environment
import Data.List
import Data.List.Split
import Debug.Trace (trace)
import Data.Maybe
import Data.Function
import Text.Read 
import Control.Monad


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

---- funkce pro druhý podúkol

-- Funkce pro rozdělení řetězce podle daného znaku
splitBy :: Char -> String -> [String]
splitBy delimiter = foldr f [[]] 
    where f c l@(x:xs) | c == delimiter = []:l
                       | otherwise = (c:x):xs

-- Funkce pro převod řetězce na řádky a jejich seřazení podle zadaného sloupce
sortLinesByColumn :: String -> Int -> String
sortLinesByColumn input column = unlines $ map snd $ sortBy (compare `on` fst) mappedLines
    where
        linesList = lines input
        splitLines = map (splitBy ',') linesList
        mappedLines = [(readMaybe (splitLines !! row !! (column - 1)) :: Maybe Double, linesList !! row) | row <- [0..length splitLines - 1]]
---- sem se to seřadí podle daného sloupce

-- Funkce pro extrakci tříd z řádků a spočítání jejich výskytů
countClasses :: [String] -> [Int]
countClasses input = map snd . map (\x -> (head x, length x)) . group . sort $ classes
  where classes = map (last . splitBy ',') input

-- Funkce pro výpočet Giniho indexu
calculateGini :: [String] -> [String] -> Double
calculateGini leftInput rightInput = let
    -- Vypočítá výskyty tříd pro oba seznamy
    leftCounts = countClasses  leftInput
    rightCounts = countClasses rightInput
    -- Vypočítá počet všech tříd
    leftTotal = sum leftCounts
    rightTotal = sum rightCounts
    -- Vypočítá Giniho index pro oba seznamy
    leftGini = 1 - sum (map (\x -> (fromIntegral x / fromIntegral leftTotal) ^ 2) leftCounts)
    rightGini = 1 - sum (map (\x -> (fromIntegral x / fromIntegral rightTotal) ^ 2) rightCounts)
    --- Vypočítá vážený průměr Giniho indexu
    weightedGini = (fromIntegral leftTotal / fromIntegral (leftTotal + rightTotal)) * leftGini + (fromIntegral rightTotal / fromIntegral (leftTotal + rightTotal)) * rightGini
    in weightedGini

-- Funkce pro rozdělení vstupních dat
splitData :: [String] -> ([String], [String])
splitData input = splitAt 1 input

-- Hlavní funkce, která iteruje a vypočítává Giniho koeficienty
calculateGiniIterations :: [String] -> [Double]
calculateGiniIterations inputData =
  let
    -- Rekurzivní funkce pro iteraci
    iterateGini :: ([String], [String]) -> [Double] -> [Double]
    iterateGini (_ , []) result = result  -- Když je pravá strana prázdná, skončíme
    iterateGini (left, right) result =
      let
        gini = calculateGini left right
        (newLeft, newRight) = moveFirstToSecond left right
      in iterateGini (newLeft, newRight) (result ++ [gini])
    
    -- Funkce pro přesun prvního prvku z pravé strany na levou
    moveFirstToSecond :: [String] -> [String] -> ([String], [String])
    moveFirstToSecond left (r:rs) = (left ++ [r], rs)
    moveFirstToSecond left [] = (left, [])  -- Tohle by se stát nemělo, ale je to pro jistotu
  in
    iterateGini (splitData inputData) []

type GiniRecord = (Double, Int, Int) -- (GiniIndex, Pozice v poli, Číslo sloupce)

-- Spočítá nejlepší Giniho index
calculateBestGini :: String -> GiniRecord
calculateBestGini inputData = 
    let allLines = lines inputData
        numColumns = length (splitBy ',' (head allLines)) -- Vyloučení sloupce s třídami
        -- Pomocná funkce pro iteraci přes sloupce a výpočet Giniho koeficientů
        iterateColumns :: Int -> GiniRecord -> GiniRecord
        iterateColumns column bestGini@(bestValue, _, _)
            | column >= numColumns = bestGini
            | otherwise =
                let sortedData = sortLinesByColumn inputData column
                    giniResults = calculateGiniIterations (lines sortedData)
                    minValue = minimum giniResults
                    minIndex = fromMaybe (-1) $ elemIndex minValue giniResults
                in if minValue < bestValue
                   then iterateColumns (column + 1) (minValue, minIndex, column)
                   else iterateColumns (column + 1) bestGini
    in iterateColumns 1 (1, -1, -1) -- Inicializace s hodnotou Gini 1, což je větší než jakákoli možná hodnota

--Funkce pro výpočet průměru mezi dvěma řádky
averageBetweenRows :: String -> (Double, GiniRecord)
averageBetweenRows inputData = 
  let
    -- Volání funkce calculateBestGini a získání výsledků
    giniRecord@(giniIndex, position, column) = calculateBestGini inputData
    -- Seřazení řádků podle zjištěného optimálního sloupce
    sortedData = sortLinesByColumn inputData column
    sortedLines = lines sortedData
    -- Získání hodnot ze zadaných řádků
    lineValue1 = read ((splitOn "," $ sortedLines !! position) !! (column-1)) :: Double
    lineValue2 = read ((splitOn "," $ sortedLines !! (position + 1)) !! (column-1)) :: Double
    -- Výpočet průměru mezi dvěma řádky
    averageValue = (lineValue1 + lineValue2) / 2
  in
    -- Vrátí průměr všech průměrů - pokud chcete jinou agregaci, upravte tuto část
    (averageValue, giniRecord)


-- Předpokládejme, že existuje funkce getClass, která vrací třídu řádku (poslední sloupec)
getClass :: String -> String
getClass = last . splitOn ","

-- Funkce pro kontrolu, zda všechny řádky v seznamu mají stejnou třídu
allSameClass :: [String] -> Bool
allSameClass rows = all (== head classes) (tail classes)
  where classes = map getClass rows

-- Pomocná funkce pro rozdělení a analýzu skupin
splitAndAnalyze :: [String] -> IO ()
splitAndAnalyze inputData = do
  unless (allSameClass inputData) $ do
    let (averageValue, (_, position, column)) = averageBetweenRows (unlines inputData)
    putStrLn $ "Průměr mezi dvěma řádky: " ++ show averageValue
    let sortedData = sortLinesByColumn (unlines inputData) column
    let sortedLines = lines sortedData
    let (leftGroup, rightGroup) = splitAt (position + 1) sortedLines
    
    putStrLn "Zpracovávám levou skupinu:"
    splitAndAnalyze leftGroup
    
    putStrLn "Zpracovávám pravou skupinu:"
    splitAndAnalyze rightGroup


splitDataByPosition :: String -> IO ()
splitDataByPosition inputData = do
  -- Rekurzivně zpracujte data a získáte levou a pravou skupinu
  processData (lines inputData) 0 -- Začněte proces s celým seznamem řádků



--Předpokládejme, že máte již implementovány ostatní potřebné funkce...

-- processData :: [String] -> Int -> IO ()
-- processData inputData depth = do
--   unless (allSameClass inputData || null inputData) $ do
--     let (averageValue, (giniIndex, position, column)) = averageBetweenRows (unlines inputData)
--     putStrLn $ replicate depth ' ' ++ "Hloubka " ++ show depth ++ ": Zpracovávám skupinu" ++ show inputData
--     putStrLn $ replicate depth ' ' ++ "Hloubka " ++ show depth ++ ": Průměr mezi dvěma řádky: " ++ show averageValue
--     putStrLn $ replicate depth ' ' ++ "Hloubka " ++ show depth ++ ": Nejlepší Gini index: " ++ show giniIndex
--     putStrLn $ replicate depth ' ' ++ "Hloubka " ++ show depth ++ ": Pozice pro rozdělení: " ++ show position
--     putStrLn $ replicate depth ' ' ++ "Hloubka " ++ show depth ++ ": Sloupec pro seřazení: " ++ show (column - 1)

--     let sortedData = sortLinesByColumn (unlines inputData) column
--     let sortedLines = lines sortedData

--     let (leftGroup, rightGroup) = splitAt (position + 1) sortedLines

--     putStrLn $ replicate depth ' ' ++ "Hloubka " ++ show depth ++ ": Zpracovávám levou skupinu"
--     processData leftGroup (depth + 1)

--     putStrLn $ replicate depth ' ' ++ "Hloubka " ++ show depth ++ ": Zpracovávám pravou skupinu"
--     processData rightGroup (depth + 1)
--   when (allSameClass inputData && not (null inputData)) $ do
--     putStrLn $ replicate depth ' ' ++ "Hloubka " ++ show depth ++ ": Všechny řádky ve skupině mají stejnou třídu " ++ getClass (head inputData)

processData :: [String] -> Int -> IO ()
processData inputData depth = do
  unless (allSameClass inputData || null inputData) $ do
    -- Předpokládejme, že averageBetweenRows je nyní upravena na IO operaci
    let (averageValue, (giniIndex, position, column)) = averageBetweenRows (unlines inputData)
    let indentation = replicate (depth * 2) ' '  -- Dvě mezery za každou úroveň hloubky
    putStrLn $ indentation ++ "Node: " ++ show (column - 1) ++ ", " ++ show averageValue

    let sortedData = sortLinesByColumn (unlines inputData) column
    let sortedLines = lines sortedData

    let (leftGroup, rightGroup) = splitAt (position + 1) sortedLines

    -- Rekurzivní zpracování levé a pravé skupiny
    processData leftGroup (depth + 1)
    processData rightGroup (depth + 1)
  when (allSameClass inputData && not (null inputData)) $ do
    let indentation = replicate (depth * 2) ' '  -- Dvě mezery za každou úroveň hloubky
    putStrLn $ indentation ++ "Leaf: " ++ getClass (head inputData)



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
            finito <- splitDataByPosition trainingDataContents
            putStrLn "Konec"

            
        _ -> putStrLn "Nespravne argumenty"
