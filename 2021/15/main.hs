import Data.List
import qualified Data.Map as Map
import Data.Ix (range)

type Coord          = (Int, Int)
type Vertex         = Coord
type Graph          = [[Distance]]
type PreviousVertex = Vertex
type Distance       = Int
type Visited        = Bool
type DijkstraVal    = (PreviousVertex, Distance, Visited)
type DijkstraEl     = (Vertex, DijkstraVal)
type Dijkstra       = Map.Map Vertex DijkstraVal

type UnvisitedList  = [(Distance, Vertex)]

getAdjacencyList :: Vertex -> [Vertex]
getAdjacencyList vertex = zipWith (\matrix vertex -> (fst matrix + fst vertex, snd matrix + snd vertex)) [(-1, 0), (1, 0), (0, -1), (0, 1)] $ replicate 4 vertex

dijkstraElAsVertex :: DijkstraEl -> Vertex
dijkstraElAsVertex el = fst el

getDistance :: DijkstraEl -> Int
getDistance (_, (_, distance, _)) = distance

visitVertex :: DijkstraEl -> DijkstraEl
visitVertex (v, (pv, d, _)) = (v, (pv, d, True))

insertUnvisited :: (Distance, Vertex) -> UnvisitedList -> UnvisitedList
insertUnvisited newVertex currentVertex = aux False False newVertex currentVertex
  where
    aux hasInserted _ newVertex []
      | hasInserted = []
      | otherwise = [newVertex]
    aux hasInserted hasRemoved newVertex@(newDistance, newCoord) (currentVertex@(currentDistance, currentCoord):ys)
      | hasRemoved && hasInserted = ys
      | (not hasInserted) && newDistance <= currentDistance = newVertex : currentVertex : aux True hasRemoved newVertex ys
      | newCoord == currentCoord = aux hasInserted True newVertex ys
      | otherwise = currentVertex : (aux hasInserted hasRemoved newVertex ys)

dijkstraElToUnlisted :: DijkstraEl -> (Distance, Vertex)
dijkstraElToUnlisted (vertex, (_, distance, _)) = (distance, vertex)

dijkstra :: Coord -> Graph -> Dijkstra
dijkstra start graph = aux [(0, start)] $ Map.fromList [(start, (start, 0, False))]
  where
    rowCount = length graph
    colCount = length (head graph)
    totalVertices = rowCount * colCount

    startingUnvisited = range ((0, 0), (rowCount - 1, colCount - 1))

    aux :: UnvisitedList -> Dijkstra -> Dijkstra
    aux unvisitedList dijkstra
      | length dijkstra == totalVertices = dijkstra
      | otherwise = aux newUnvisitedList newDijkstra
      where
        nextVertex :: DijkstraEl
        nextVertex = (vertex, dijkstra Map.! vertex)
          where
            vertex = snd $ head unvisitedList

        newUnvisitedList :: UnvisitedList
        newUnvisitedList = foldl' (flip insertUnvisited) (tail unvisitedList) $ filter (\(_, coord) -> Map.notMember coord dijkstra) $ map dijkstraElToUnlisted neighbours

        visitedVertex :: DijkstraEl
        visitedVertex = visitVertex nextVertex

        isMember neighbour = Map.member neighbour dijkstra

        getNeighbours :: [Vertex] -> [DijkstraEl]
        getNeighbours [] = []
        getNeighbours (v:vs)
          | validIndex = (v, (dijkstraElAsVertex nextVertex, (getDistance nextVertex) + currentNeighbourWeight, False)) : (getNeighbours vs)
          | otherwise = getNeighbours vs
          where
            row = fst v
            col = snd v
            validIndex = row >= 0 && row < rowCount && col >= 0 && col < colCount
            currentNeighbourWeight = graph !! row !! col

        neighbours = getNeighbours $ getAdjacencyList $ dijkstraElAsVertex nextVertex

        newDijkstra :: Dijkstra
        newDijkstra = Map.insert (fst visitedVertex) (snd visitedVertex) $ foldl' (\acc neighbour -> insert neighbour acc) dijkstra neighbours
          where
          insert :: DijkstraEl -> Dijkstra -> Dijkstra
          insert (v, (pv, d, visited)) dijkstra
            | isMember v = Map.insertWith (\acc@(accPv, accD, accVisited) (newPv, newD, newVisited) -> if accD <= newD then acc else (newPv, newD, accVisited || newVisited)) v (pv, d, visited) dijkstra
            | otherwise = Map.insert v (pv, d, visited) dijkstra

findDistance :: DijkstraVal -> Int
findDistance (_, d, _) = d

solve :: Graph -> String
solve graph = show $ findDistance (dijkstraResults Map.! end)
  where
    rowCount = length graph
    colCount = length (head graph)

    dijkstraResults :: Dijkstra
    dijkstraResults = dijkstra (0, 0) graph

    end = (rowCount - 1, colCount - 1)

printBoard :: Graph -> String
printBoard = unlines . map (map (head . show))

extendBoardDown :: Int -> Graph -> Graph
extendBoardDown rowsPer board = board ++ (map (map inc) (drop ((length board)- rowsPer) board))
  where
    inc :: Int -> Int
    inc val
      | val == 9 = 1
      | otherwise = val + 1

extendBoardRight :: Int -> Graph -> Graph
extendBoardRight colsPer board = map (\row -> row ++ (map inc (drop ((length row) - colsPer) row))) board
  where
    inc :: Int -> Int
    inc val
      | val == 9 = 1
      | otherwise = val + 1

biggerBoard :: Graph -> Graph
biggerBoard board = aux 5 board
  where
    aux :: Int -> Graph -> Graph
    aux multiplier auxBoard
      | multiplier <= 1 = auxBoard
      | otherwise = aux (multiplier - 1) $ (extendBoardDown (length board)) $ (extendBoardRight (length (head board))) auxBoard

main :: IO ()
main = do
   contents <- readFile "input.txt"
   let input =  map (map ((read :: String -> Int) . (:[]))) $ lines contents
   putStrLn $ "Part 1: " ++ (solve input)
   putStrLn $ "Part 2: " ++ (solve (biggerBoard input))
