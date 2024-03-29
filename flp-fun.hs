import System.IO()
import System.Environment
import Data.List
import Data.List.Split
import Debug.Trace()
import Data.Maybe
import Data.Function()
import Text.Read()
import Data.Ord

type NodeData = (String, Int, String, Int)

-- Čte text ze souboru
loadFile :: FilePath -> IO String
loadFile filePath = do
    contents <- readFile filePath
    return contents

-- Přidává odsazení řetězce pro zjištění, ke kterému uzlu co patří
addIndent :: [String] -> [(String, Int)]
addIndent strtings = map createIntednt strtings where
  createIntednt str = (str, length (takeWhile (==' ') str) `div` 2) -- vydělí počet mezer 2 (dle zadání je jedna úroveň 2 mezery)

-- Zjistí, jestli je uzel nebo list
whatNode :: String -> String
whatNode str
  | "Node" `isPrefixOf` str = "Node"
  | otherwise = "Leaf"

getNumber :: String -> [[Char]] -> Int
getNumber str restString
  | "Node" `isPrefixOf` str = read (head restString) :: Int -- Uzel má index na prvním místě (["0", "690.585"])
  | otherwise = -1

getValue :: String -> [[Char]] -> String
getValue str restString
  | "Node" `isPrefixOf` str = restString !! 1 -- Uzel má hodnotu na druhém místě (["0", "690.585"])
  | otherwise = head restString


createNodeData :: (String, Int) -> NodeData
createNodeData (str, indent) = -- str je něco jako "Node: 0, 690.585"
  let wrds = words str -- rozdělím na ["Node:","0,","690.585"]
      first = head wrds -- Typ uzlu
      rest = tail wrds -- Zbytek
      nodeType = whatNode first -- Zjištění typu uzlu/odstranění ':'
      restString = map (filter (/=',')) rest -- Odstranění ',' z čísel (abych měl něco jako ["0", "690.585"] místo ["0,", "690.585"])
      number = getNumber nodeType restString -- Získá index příznaku uzlu
      value = getValue nodeType restString -- Získá název listu/hodnotu ulzu
  in (nodeType, number, value, indent)

-- Rovnou převede vstupní data ze stringu na čísla
findLeaf :: [NodeData] -> [String] -> String
findLeaf nodes numbers = findLeafIdk nodes (map read numbers :: [Double]) 0 0 -- Pošlu sem všechny uzly a čísla ze vstupu, první 0 značí, že nebudu skipovat nic, druhá 0 je odsazení, které hledám (0 je kořen)

findLeafIdk :: [NodeData] -> [Double] -> Int -> Int -> String
findLeafIdk [] _ _ _ = error "Nenalezeno"
findLeafIdk ((typ, index, value, indent):xs) numbers skips indentToFind
  | typ == "Node" && comparisonNumber >= read value && skips == 0 && indent == indentToFind = 
      findLeafIdk xs numbers 1 (indent + 1) -- Pokud je hodnota na vstupu vyšší, než hodnota uzlu, posouvám se ve stromu doprava = hledám až 2. výskyt uzlu s odsazením + 1
  | typ == "Node" && comparisonNumber < read value && skips == 0 && indent == indentToFind = 
      findLeafIdk xs numbers 0 (indent + 1) -- Pokud je hodnota na vstupu nižší, než hodnota uzlu, posouvám se ve stromu doleva = hledám 1. výskyt uzlu s odsazením + 1
  | typ == "Leaf" && skips > 0 && indent >= indentToFind = 
      findLeafIdk xs numbers (skips - 1) indentToFind -- Pokud možná jsem našel list na správné úrovní, ale mám ještě skipovat = hledám dál, ale příště už neskipuju
  | typ == "Leaf" && skips == 0 && indent == indentToFind = 
      value -- našel jsem list na správné úrovni a nemám skipovat = vracím jeho název
  | otherwise = findLeafIdk xs numbers skips indentToFind -- Pokud nic z toho neplatí, hledám dál
  where comparisonNumber = numbers !! index -- Čísla se neberou postupně ze vstupu, ale podle indexu příznaku uzlu

---- funkce pro druhý podúkol

-- rovnou si vytvořím něco jako (857.88, "857.88,230.55,1041.12,498.01,1037.92,654.19,612.74,903.97,1026.55,147.14,Class10") pro 1
getColValue :: Int -> String -> [(Double, String)]
getColValue column input = 
    map fixInput (lines input)
    where
      fixInput row = 
        let values = splitOn "," row -- rozdělím na hodnoty
            key = read (head (drop (column-1) values)) :: Double -- vezmu číslo na indexu
        in (key, row) -- vrátím dvojici

-- seřadím řádky a vrátím zas jak jeden dlouhej string
sortLinesByColumn :: String -> Int -> String
sortLinesByColumn input column =
    let sortedRows = sortBy (comparing fst) $ getColValue column input
    in unlines $ map snd sortedRows

-- sem se to seřadí podle daného sloupce

-- vezmu název třídy
getLastCol :: String -> String
getLastCol input = reverse . takeWhile (/=',') $ reverse input -- abych nehledal poslední sloupec, otočím řádek a vezmu dokud nenarazím na ','

countOccurrences :: Eq a => [a] -> [Int]
countOccurrences xs = map count (nub xs)
  where
    count x = length (filter (== x) xs) -- spočítám počet výskytů x v původním seznamu

---- všechno možný pro výpočet gini indexu a výběr nejlepšího prahu
countClasses :: [String] -> [Int]
countClasses input = let
    classes = map getLastCol input -- vytvoří něco takovýho ["Class10","Class4","Class4","Class7","Class6","Class6","Class3","Class10","Class1","Class5"]
    countedClasses = countOccurrences classes -- [1,2,1,2,1,2,1]
    in countedClasses -- [1,2,1,2,1,2,1]

calculateGiniSquare :: Int -> Int -> Double
calculateGiniSquare total count = (fromIntegral count / fromIntegral total) ** 2.0

-- V podstatě si rozdělím vstup na 2 skupiny, podle toho spočítám gini index a vrátím vážený průměr
calculateGini :: [String] -> [String] -> Double
calculateGini leftInput rightInput = let
    leftCounts = countClasses leftInput -- spočítám, kolik je jednotlivých tříd v levé skupině
    rightCounts = countClasses rightInput -- počet jednotlivých tříd v pravé skupině
    leftTotal = sum leftCounts -- celkově položek v levé skupině
    rightTotal = sum rightCounts -- celkově položek v pravé skupině
    leftGini = 1 - sum (map (calculateGiniSquare leftTotal) leftCounts) -- spočítám gini index levé skuoiny
    rightGini = 1 - sum (map (calculateGiniSquare rightTotal) rightCounts) -- spočítám gini index pravé skupiny
    weightedGini = (fromIntegral leftTotal / fromIntegral (leftTotal + rightTotal)) * leftGini + (fromIntegral rightTotal / fromIntegral (leftTotal + rightTotal)) * rightGini -- vážený průměr dle vzorečku
    in weightedGini


-- Přesunu řádek z pravé skupiny do levé
moveRightToLeft :: [String] -> [String] -> ([String], [String])
moveRightToLeft left (r:rs) = (left ++ [r], rs)
moveRightToLeft _ [] = error "neco se pokazilo"

-- Přesouvám řádky a počítám Gini index
iterateGini :: ([String], [String]) -> [Double]
iterateGini (_, []) = []
iterateGini (left, right) =
  let
    gini = calculateGini left right -- spočítám gini index pro tohle rozdělení
    moved = moveRightToLeft left right -- přesunu řádek z pravé skupiny do levé
  in gini : iterateGini moved -- vytvářím seznam gini indexů pro všechny možný rozdělení

calculateGiniIterations :: [String] -> [Double]
calculateGiniIterations inputData = iterateGini (splitAt 1 inputData)

type GiniRecord = (Double, Int, Int) -- (GiniIndex, Pozice v poli, Číslo sloupce)

-- Hlavní funkce pro výpočet nejlepšího Giniho koeficientu
calculateBestGini :: String -> GiniRecord
calculateBestGini inputData =
    let allLines = lines inputData
        numColumns = length (splitOn "," (head allLines))
    in iterateColumns inputData 1 numColumns (1, -1, -1) -- Začnu s hodnotou indexu 1, protože vyšší už nic nebude (snad)

iterateColumns :: String -> Int -> Int -> GiniRecord -> GiniRecord
iterateColumns inputData column numColumns bestGini
    | column >= numColumns = bestGini -- pokud už jsem na sloupci s názvama tříd, nepokračuju dál
    | let (lowestGini, _, _) = bestGini, lowestGini == 0 = bestGini -- Pokud je gini index 0, nebudeme iterovat dál
    | otherwise =
        let (lowestGini, _, _) = bestGini -- vezmu nejmenší index
            sortedData = sortLinesByColumn inputData column
            giniResults = calculateGiniIterations (lines sortedData)
            minValue = minimum giniResults -- spočítám nový nejmenší index
            minIndex = fromMaybe (-1) (elemIndex minValue giniResults) -- uložím pozici nového nejmenšího indexu, musím použít fromMaybe, protože elemIndex mi vrací Maybe Int
        in if minValue == 0 -- pokud je nový index 0, rovnou končím
           then (0, minIndex, column)
           else if minValue < lowestGini
                then iterateColumns inputData (column + 1) numColumns (minValue, minIndex, column) -- pokud je nový index menší, stane se z něj bestGini
                else iterateColumns inputData (column + 1) numColumns bestGini

-- TODO

-- truncate' :: Double -> Int -> Double -- ze SO
-- truncate' x n = (fromIntegral (floor (x * t)) :: Double) / t -- zaokrouhlím na n desetinných míst
--     where t = 10^(n :: Int)

averageBetweenRows :: String -> (Double, GiniRecord)
averageBetweenRows inputData = 
  let

    (_, position, column) = calculateBestGini inputData -- zjistím, kde je nejlepší index
    sortedData = sortLinesByColumn inputData column -- seřadím data podle sloupce
    sortedLines = lines sortedData
    lineValue1 = read (head(drop (column-1) (splitOn "," (head (drop position sortedLines))))) :: Double -- získám hodnotu prvního ze dvou řádků
    lineValue2 = read (head(drop (column-1) (splitOn "," (head (drop (position + 1) sortedLines))))) :: Double -- hodnota druhýho řádku
    averageValueBefore = (lineValue1 + lineValue2) / 2 -- průměr těch dvou hodnot
    --averageValue = truncate' averageValueBefore 3 -- podle testů zaokrouhlím na 3 desetinná
  in
    (averageValueBefore, (0.0, position, column)) -- vrátím průměr mezi dvěma řádkama

allSameClass :: [String] -> Bool
allSameClass rows = 
  let 
    classes = map getLastCol rows -- vezmu názvy tříd
  in 
    all (== head classes) (tail classes) -- zjistím, jestli jsou všechny třídy stejné


printTree :: [String] -> Int -> IO ()
printTree inputData depth =
  if allSameClass inputData then
    putStrLn (replicate (depth * 2) ' ' ++ "Leaf: " ++ getLastCol (head inputData)) -- vytisknu list s mezerama podle hloubky depth
  else do
    let (averageValue, giniRecord) = averageBetweenRows (unlines inputData) -- zjistím nejlepší threshold
    let (_, position, column) = giniRecord -- zjistím pozici a sloupec pro další rozdělení a správnej výtisk
    putStrLn (replicate (depth * 2) ' ' ++ "Node: " ++ show (column - 1) ++ ", " ++ show averageValue) -- dost mezer, Node, sloupec nejlepšího gini indexu, threshold
    
    let sortedData = sortLinesByColumn (unlines inputData) column
    let sortedLines = lines sortedData
    let (leftGroup, rightGroup) = splitAt (position + 1) sortedLines -- rozdělím na levou a pravou skupinu == podstromy
    
    printTree leftGroup (depth + 1)
    printTree rightGroup (depth + 1)


main :: IO ()
main = do
    args <- getArgs
    case args of
        ("-1":treeFile:dataFile:_) -> do -- První podúkol
            treeContent <- loadFile treeFile
            dataContent <- loadFile dataFile
            let transformedList = map createNodeData (addIndent (lines treeContent)) -- Každý uzel bude mít tento tvar: ("Node",0,"690.585",0) aka typ, index, hodnota, indent
            let fixedDataContent = map (splitOn ",") (lines dataContent) -- Rozdělím data na řádky a převedu každý na seznam
            let result = map (findLeaf transformedList) fixedDataContent
            mapM_ putStrLn result
        ("-2":dataFile:_) -> do -- Druhý podúkol
            dataContent <- loadFile dataFile
            printTree (lines dataContent) 0
        _ -> putStrLn "Nespravne argumenty"
