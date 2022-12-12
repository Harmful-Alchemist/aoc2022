import Data.Char
import Data.Map (Map, fromList, (!), toList, member)
import GHC.CmmToAsm.CFG (addEdge)
import Data.List

day12 = do
    content <- readFile "input12.txt"
    let coords = parse12' content
    let mapped = toMap coords
    let start = getCoord $ head $ filter (\n -> getType n == Start) coords
    let end = getCoord $ head $ filter (\n -> getType n == End) coords  --TODO get the end coord here
    let withOutgoingEdges = addEdges mapped
    print $ dfsish' start mapped end
    return ()


data Type = Start|End|Regular
    deriving (Show, Eq)
type Coordinate = (Int,Int)
data Node = N Coordinate Int Type [Coordinate]
    deriving Show


parse12' s = parse12 s [] (0,0)

parse12 :: String -> [Node] -> Coordinate -> [Node]
parse12 [] acc _ = acc
parse12 (c:xs) acc (x,y) = if c == '\n' then  parse12 xs (newNode:acc) (0,y+1) else parse12 xs (newNode:acc) (x+1,y)
    where newNode = case c of
            'S' -> N (x,y) (ord 'a') Start []
            'E' -> N (x,y) (ord 'z') End []
            _   -> N (x,y) (ord c)  Regular []

toMap :: [Node] -> Map Coordinate Node
toMap = fromList . map (\(N c i t ns) -> (c,N c i t ns))

addEdges :: Map Coordinate Node -> Map Coordinate Node
addEdges mapped = fromList $ map (mapFn mapped) $ toList mapped

mapFn :: Map Coordinate Node -> ((Int, Int), Node) -> ((Int, Int), Node)
mapFn mapped ((x,y),N c i t ns) = ((x,y),N c i t edges)
    where adj = filter (`member` mapped) [(x+1,y), (x,y+1), (x,y+1), (x,y-1)]
          edges = filter (\coord -> let (N _ i' _ _) = (mapped ! coord) in i' == i +1 || i' == i) adj
    -- where option = [(x',y') | x' <- [(x-1)..(x+1)], y<-[(y-1)..(y+1)], guards! ] not diag!

getCoord :: Node -> Coordinate
getCoord (N c i t ns) = c

getType :: Node -> Type
getType (N c i t ns) = t

getConnected :: Node -> [Coordinate]
getConnected (N c i t ns) = ns

dfsish' :: Coordinate ->  Map Coordinate Node -> Coordinate -> Int
dfsish' start = dfsish [start] 0
--start always (0,0)?

dfsish :: [Coordinate] -> Int -> Map Coordinate Node -> Coordinate ->  Int
dfsish visited roundNo mapped end | end `elem` visited = roundNo
                                  | otherwise          = dfsish (map head . group . sort $ connected)  (roundNo+1) mapped end
                                    where connected = concatMap (getConnected . (!) mapped) visited
