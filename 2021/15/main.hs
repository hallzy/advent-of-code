import Data.List
import qualified Data.Map as Map
import Data.Ix (range)

type Coord          = (Int, Int)
type Vertex         = Coord
type Graph          = [[Distance]]
type PreviousVertex = Vertex
type Distance       = Int
type Visited        = Bool
type Dijkstra       = [(Vertex, PreviousVertex, Distance, Visited)]

-- TODO: Dijkstra might be better as a Map on the Vertex (vertex can be removed
-- from the map value then)

-- TODO: Graph could be a map too. Maybe leave it for now unless I end up with
-- performance issues

-- Dijkstra
-- Keep Track of the nodes you have visited, and the nodes you haven't visited
-- Set the distance to your starting node as 0, because you are already there
-- The distance to all other nodes is inf until we evaluate them

-- Set distance from start to start as 0
-- Previous vertex is null

-- Visit the unvisited vertex with the lowest distance (initially, this is just
-- the start vertex)

-- Loop starts here

-- Examine the current vertex's unvisited neighbours

-- Calculate the distance to those neighbours from the current vertex. The
-- distance is the distance from the current vertex to the neighbour, plus the
-- distance assigned to the current vertex.

-- If the calculated distance is lower than the current calculated distance for
-- that vertex, then replace the distance, and set the current vertex as the
-- "previous vertex" for that neighbour.

-- Add the current vertex to the list of unvisited vertices and remove it from
-- the unvisited list

-- repeat

-- Algorithm is done when there are no more unvisited vertices

dijkstra :: Coord -> Graph -> Dijkstra
dijkstra start graph = aux [(start, start, 0, False)]
  where
    rowCount = length graph
    colCount = length (head graph)
    totalVertices = rowCount * colCount

    startingUnvisited = range ((0, 0), (rowCount - 1, colCount - 1))

    aux :: Dijkstra -> Dijkstra
    aux dijkstra
      | length dijkstra == totalVertices = dijkstra
      | otherwise = aux newDijkstra
      where
        nextVertex = foldl1' (\acc@(_, _, accDistance, _) new@(_, _, newDistance, _) -> if accDistance <= newDistance then acc else new) $ filter (\(_, _, _, visited) -> not visited) dijkstra
        neighbours = undefined -- Get the neighbours of nextVertex with the updated distance
        newDijkstra = undefined -- Add the neighbours to newDijkstra and update the distance and previous vertex

solve1 :: Graph -> String
solve1 graph = show $ dijkstra (0, 0) graph

main :: IO ()
main = do
   contents <- readFile "input.txt"
   let input =  map (map ((read :: String -> Int) . (:[]))) $ lines contents
   putStrLn $ "Part 1: " ++ (solve1 input)
