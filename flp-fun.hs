import System.IO()
import System.Environment
import Data.List
import Data.List.Split
import Debug.Trace()
import Data.Maybe
import Data.Function()
import Text.Read()
import Control.Monad
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
            --key = read (values !! (column - 1)) :: Double -- vvezmu číslo na indexu
            key = read (head (drop (column-1) values)) :: Double
            --key = read (head(drop (column-1) values)) :: Double -- vvezmu číslo na indexu
        in (key, row) -- vrátím dvojici

-- Funkce pro seřazení řádků podle hodnoty ve specifikovaném sloupci a jejich spojení do jednoho řetězce
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
    count x = length (filter (== x) xs)

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

-- TODO

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
    iterateGini (splitAt 1 inputData) []

type GiniRecord = (Double, Int, Int) -- (GiniIndex, Pozice v poli, Číslo sloupce)

-- Spočítá nejlepší Giniho index
calculateBestGini :: String -> GiniRecord
calculateBestGini inputData = 
    let allLines = lines inputData
        numColumns = length (splitOn "," (head allLines))
        -- Pomocná funkce pro iteraci přes sloupce a výpočet Giniho koeficientů
        iterateColumns :: Int -> GiniRecord -> GiniRecord
        iterateColumns column bestGini@(bestValue, _, _)
            | column >= numColumns = bestGini
            | bestValue == 0 = bestGini  -- Kontrola, zda již byl nalezen nejlepší možný výsledek
            | otherwise =
                let sortedData = sortLinesByColumn inputData column
                    giniResults = calculateGiniIterations (lines sortedData)
                    minValue = minimum giniResults
                in if minValue == 0 
                   then (0, fromMaybe (-1) $ findIndex (==0) giniResults, column)
                   else let minIndex = fromMaybe (-1) $ elemIndex minValue giniResults
                        in if minValue < bestValue
                           then iterateColumns (column + 1) (minValue, minIndex, column)
                           else iterateColumns (column + 1) bestGini
    in iterateColumns 1 (1, -1, -1) -- Inicializace s hodnotou Gini 1, což je větší než jakákoli možná hodnota

--Funkce pro výpočet průměru mezi dvěma řádky
averageBetweenRows :: String -> (Double, GiniRecord)
averageBetweenRows inputData = 
  let
    -- Volání funkce calculateBestGini a získání výsledků
    giniRecord@(_, position, column) = calculateBestGini inputData
    -- Seřazení řádků podle zjištěného optimálního sloupce
    sortedData = sortLinesByColumn inputData column
    sortedLines = lines sortedData
    -- Získání hodnot ze zadaných řádků
    lineValue1 = read ((splitOn "," $ sortedLines !! position) !! (column-1)) :: Double
    lineValue2 = read ((splitOn "," $ sortedLines !! (position + 1)) !! (column-1)) :: Double
    -- Výpočet průměru mezi dvěma řádky
    averageValueBefore = (lineValue1 + lineValue2) / 2
    averageValue = (fromIntegral (round (averageValueBefore * 1000)) :: Double) / 1000
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


splitDataByPosition :: String -> IO ()
splitDataByPosition inputData = do
  -- Rekurzivně zpracujte data a získáte levou a pravou skupinu
  processData (lines inputData) 0 -- Začněte proces s celým seznamem řádků



processData :: [String] -> Int -> IO ()
processData inputData depth = do
  unless (allSameClass inputData || null inputData) $ do
    -- Předpokládejme, že averageBetweenRows je nyní upravena na IO operaci
    let (averageValue, (_, position, column)) = averageBetweenRows (unlines inputData)
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
        ("-1":treeFile:dataFile:_) -> do -- První podúkol
            treeContent <- loadFile treeFile
            dataContent <- loadFile dataFile
            let transformedList = map createNodeData (addIndent (lines treeContent)) -- Každý uzel bude mít tento tvar: ("Node",0,"690.585",0) aka typ, index, hodnota, indent
            let fixedDataContent = map (splitOn ",") (lines dataContent) -- Rozdělím data na řádky a převedu každý na seznam
            let result = map (findLeaf transformedList) fixedDataContent
            mapM_ putStrLn result
        ("-2":dataFile:_) -> do -- Druhý podúkol
            dataContent <- loadFile dataFile
            splitDataByPosition dataContent
        _ -> putStrLn "Nespravne argumenty"
