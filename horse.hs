import System.IO
import System.Environment (getArgs)
import Control.Monad
import Data.List
import Data.List (sortBy)
import Data.Ord (comparing)


type Position = (Int, Int)
type Path = [Position]

main :: IO ()
main = do
    args <- getArgs
    contents <-
        if null args
            then do
                putStrLn $ "Entre o nome do arquivo(texto): "
                fileName <- getLine
                readFile fileName
            else do
                putStrLn ("Received arguments: " ++ show args)
                let fileName = head args
                readFile fileName

    let allLines = lines contents
    mapM_ processLine allLines

processLine :: String -> IO ()
processLine line = do
    -- separar nos espacos
    let partes = words line
    -- parsa como ints(sem validacao se der erro)
    let numbers = map read partes :: [Int]

    case numbers of -- processa linha
        [m, n, x, y] -> do
            putStrLn $ "Result for " ++ show numbers
            let firstSquare = (x, y)
    
            let t_empty = buildMatrix (m, n) -- cria matriz
    
            let t_initial = setMatrixValue firstSquare (-1) t_empty --casa inicial ja visitada
        
            let sortedOptions = list_options firstSquare t_initial -- calcula candidatos pro primeiro mov
            let nextMoves = map fst sortedOptions

            let result = tryOptions nextMoves t_initial [firstSquare] -- calcula melhor candidato
    
            case result of
                Just path -> do -- resolvido
                    putStrLn "Soluçao encontrada!"
                    putStrLn "Caminho:"
                    print path
                Nothing -> -- nao resolvido
                    putStrLn "Nao foi possível encontrar uma soluçao."


buildMatrix :: (Int, Int) -> [[Int]]
buildMatrix (m, n) = 
    replicate m (replicate n 0)


setMatrixValue :: (Int, Int) -> Int -> [[Int]] -> [[Int]] -- matriz nxn
setMatrixValue (r, c) newValue matrix =
    let (beforeRows, currentRow:afterRows) = splitAt r matrix
        (beforeCols, _:afterCols) = splitAt c currentRow
        newRow = beforeCols ++ (newValue : afterCols)
    in beforeRows ++ (newRow : afterRows)


isValid :: [[Int]] -> (Int, Int) -> Bool
isValid matrix (r, c) =
    let m = length matrix
        n = if m == 0 then 0 else length (head matrix)
    in
        r >= 0 && r < m &&
        c >= 0 && c < n &&
        (matrix !! r) !! c == 0

isFull :: [[Int]] -> Bool
isFull t = all (/= 0) (concat t)

isReachable :: (Int, Int) -> (Int, Int) -> Bool
isReachable (x1, y1) (x2, y2) =
    let dx = abs (x1 - x2)
        dy = abs (y1 - y2)
    in (dx == 2 && dy == 1) || (dx == 1 && dy == 2)


verify_neighbors :: (Int, Int) -> [[Int]] -> [(Int, Int)]
verify_neighbors (x, y) matrix =
    let deltas = [ (1, 2), (1, -2), (-1, 2), (-1, -2)
                 , (2, 1), (2, -1), (-2, 1), (-2, -1) ]
        potentialMoves = map (\(dx, dy) -> (x + dx, y + dy)) deltas
    in filter (isValid matrix) potentialMoves


list_options :: (Int, Int) -> [[Int]] -> [((Int, Int), (Int, Int))]
list_options (x, y) t =
    let neighbors = verify_neighbors (x, y) t
        countNeighbors pos = length (verify_neighbors pos t) -- calcula o numero de movimentos possiveis apartir de uma casa
        lookahead pos = -- calcula o numero minimo de movimentos possiveis apartir de uma casa X alcançavel pela casa pos
            let next = verify_neighbors pos t
            in if null next
               then maxBound
               else minimum (map countNeighbors next)
        pairs = map (\pos -> (pos, (countNeighbors pos, lookahead pos))) neighbors -- mapeamos neighbors para uma lista de tuplas
    in sortBy (comparing snd) pairs --                                             contendo o valor original(pos),
--                                                                                 a quantidade de casas atingiveis apartir de pos,
--                                                                                 e o minimo de casas atingivel apartir de uma casa
--                                                                                 atingivel por pos

tourRecursive :: Position     
              -> Position      
              -> [[Int]]       -- board
              -> Path          -- caminho
              -> Maybe Path    
tourRecursive startPos (x, y) t path =
    let t_new = setMatrixValue (x, y) 1 t
        newPath = path ++ [(x,y)]
    in
    if isFull t_new
    then
        Just newPath
    else
        let sortedOptions = list_options (x, y) t_new
            nextPositions = map fst sortedOptions
        in tryOptions nextPositions t_new newPath

tryOptions :: [Position] -> [[Int]] -> Path -> Maybe Path
tryOptions [] _ _ = Nothing
tryOptions (p:ps) t path =
    case tourRecursive (head path) p t path of
        Just solution -> Just solution
        Nothing       -> tryOptions ps t path


