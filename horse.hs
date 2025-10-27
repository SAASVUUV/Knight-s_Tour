import Data.List (sortBy)
import Data.Ord (comparing) 

main :: IO()
main = do
    l <- getLine
    let (m, n, x, y) = parseLine l
    let firstSquare = (x, y)
    
    let t_empty = buildMatrix (m, n)
    
    let t_initial = setMatrixValue firstSquare (-1) t_empty
        
    let sortedOptions = list_options firstSquare t_initial
    let nextMoves = map fst sortedOptions

    let success = or (map (\move -> tourRecursive firstSquare move t_initial) nextMoves)
    
    if success
    then putStrLn "Solução encontrada!"
    else putStrLn "Não foi possível encontrar uma solução."




parseLine :: String -> (Int, Int, Int, Int)
parseLine "" = (0, 0, 0, 0)
parseLine l = 
    let numbers = map read (words l)
    in (numbers !! 0, numbers !! 1, numbers !! 2, numbers !! 3)


buildMatrix :: (Int, Int) -> [[Int]]
buildMatrix (m, n) = 
    replicate m (replicate n 0)


setMatrixValue :: (Int, Int) -> Int -> [[Int]] -> [[Int]]
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


list_options :: (Int, Int) -> [[Int]] -> [((Int, Int), Int)]
list_options (x, y) t =
    let neighbors = verify_neighbors (x, y) t
        countNeighbors pos = length (verify_neighbors pos t)
    in sortBy (comparing snd) $
       map (\pos -> (pos, countNeighbors pos)) neighbors


tourRecursive :: (Int, Int) -> (Int, Int) -> [[Int]] -> Bool
tourRecursive startPos (x, y) t =
    let t_new = setMatrixValue (x, y) 1 t
    
    in if isFull t_new
       then 
           not (isReachable (x, y) startPos)
       else 
           let sortedOptions = list_options (x, y) t_new
               nextMoves = map fst sortedOptions
           in or (map (\move -> tourRecursive startPos move t_new) nextMoves)