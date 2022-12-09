import Data.List (intersect, sort, elemIndex, isSubsequenceOf, group)
import Data.Char (ord, isUpper, digitToInt, isDigit)
import Data.Maybe (fromJust)

day1 = do
    content <- readFile "input1.txt"
    let bundled = bundle (lines content) [] []
    print (findMax bundled)
    print $ top3 bundled
    return ()

bundle :: [String] -> [String] -> [[String]] -> [[String]]
bundle [] _ as = as
bundle ("":xs) a as = bundle xs [] (a:as)
bundle (x:xs) a as = bundle xs (x:a) as

parse1 :: [[String]] -> [[Int]]
parse1 = map (map read)

total :: [[String]] -> [Int]
total = map sum . parse1

top3 :: [[String]] -> Int
top3 xss = let (x:y:z:_) = reverse $ sort $ total xss in x + y +z

findMax :: [[String]] -> Int
findMax = maximum . total

-- day 2

day2 = do
    content <- readFile "input2.txt"
    let bundled = lines content
    print $ sum $ map (scoreRound . parseRound) bundled
    print $ sum $ map (scoreRound . toDesiredPlay . parseRound) bundled
    return ()

data Play = Rock | Paper | Scissors
    deriving Eq

toValue :: Play -> Int
toValue Rock = 1
toValue Paper = 2
toValue Scissors = 3

toDesiredResult :: Play -> Ordering
toDesiredResult Rock = LT
toDesiredResult Paper = EQ
toDesiredResult Scissors = GT

toDesiredPlay :: (Play, Play) -> (Play, Play)
toDesiredPlay (opp, yours) = (opp, desiredPlay)
    where cmp = [(compare x opp, x) | x <- [Rock, Paper, Scissors]]
          desiredPlay = case lookup (toDesiredResult yours) cmp of
            Just x -> x
            Nothing -> error "No"


fromChar :: Char -> Play
fromChar x | x == 'A' || x == 'X' = Rock
             | x == 'B' || x == 'Y' = Paper
             | otherwise            = Scissors

instance Ord Play where
  -- compare :: Play -> Play -> Ordering
  compare Rock Rock = EQ
  compare Rock Paper = LT
  compare Rock Scissors = GT
  compare Paper Rock = GT
  compare Paper Paper = EQ
  compare Paper Scissors = LT
  compare Scissors Rock = LT
  compare Scissors Paper = GT
  compare Scissors Scissors = EQ

scoreRound :: (Play, Play) -> Int
scoreRound (opponent, yours) = playVal + scoreVal
            where playVal = toValue yours
                  scoreVal = case compare yours opponent of
                    GT -> 6
                    EQ -> 3
                    LT -> 0

parseRound :: String -> (Play, Play)
parseRound (opp : ' ' : yours : _ ) = (fromChar opp, fromChar yours)
parseRound _ = error "No!"

-- day 3
day3 = do
    content <- readFile "input3.txt"
    let bundled = lines content
    print $ sum $ map (valueItem . intersecting . split2) bundled
    print $ sum $ map (valueItem . intersecting3) $ group3 bundled
    return ()

split2 :: [a] -> ([a], [a])
split2 x = splitAt len x
    where len = length x `div` 2

intersecting (x,y) = x `intersect` y
intersecting3 (x,y, z) = x `intersect` y `intersect` z

valueItem :: String -> Int
valueItem xs = if isUpper hd then ord hd -38 else ord hd -96
    where hd = head xs

group3 :: [String] -> [(String, String, String)]
group3 [] = []
group3 (x:y:z:xs) = (x,y,z): group3 xs
group3 _ = error "Nah"


-- day 4
day4 = do
    content <- readFile "input4.txt"
    let bundled = lines content
    print $ length $ filter completeOverLap $ map parseLine bundled
    print $ length $ filter anyOverlap $ map parseLine bundled
    return ()

completeOverLap :: ([Int], [Int]) -> Bool
completeOverLap (xs,ys) | length xs < length ys = isSubsequenceOf xs ys
                        | otherwise = isSubsequenceOf ys xs

anyOverlap :: ([Int], [Int]) -> Bool
anyOverlap (xs,ys) = not (null (xs `intersect` ys))

parseLine :: String -> ([Int], [Int])
parseLine xs = let (a,b) = splitOn ',' xs in (parsePart a,parsePart b)

parsePart :: String -> [Int]
parsePart xs = let (x',y') = splitOn '-' xs in (let  (x, y) = (toInt 0 x', toInt 0 y') in [x..y])

splitOn :: Char -> String -> (String, String)
splitOn  x xs = (a,b)
    where splitted = elemIndex x xs
          a = take (fromJust splitted) xs
          b = drop (fromJust splitted + 1) xs

toInt :: Int -> String -> Int
toInt x ys = foldl (\ x y -> 10 * x + digitToInt y) x ys

--day 5
day5 = do
    content <- readFile "input5.txt"
    -- content <- readFile "ex5.txt"
    let bundled = lines content
    let (cmds, toParse) = parseCommands (reverse bundled) []
    let emptyStacks = createStacks (toParse !! 1)
    let stacks = addCrates (drop 1 toParse) emptyStacks
    let processed = eval reverse (reverse cmds) stacks
    print $ tops processed
    let processed2 = eval id (reverse cmds) stacks
    print $ tops processed2
    return ()

data Cmd = C Int Int Int
    deriving Show
-- C amount from to

parseCommands :: [String] -> [Cmd] -> ([Cmd], [String])
parseCommands ([]:stackInfo) cmds = (cmds, stackInfo)
parseCommands (x:xs) cmds = parseCommands xs (parseCommand x (C 0 0 0) : cmds)
parseCommands _ _ = error "-"

parseCommand :: String -> Cmd -> Cmd
parseCommand ('m':_:_:_:_: xs) (C x y z) = let (x', xs') =  parseNo xs 0 in parseCommand xs' (C x' y z)
parseCommand (_:'f':_:_:_:_: xs) (C x y z) = let (y', xs') =  parseNo xs 0 in parseCommand xs' (C x y' z)
parseCommand (_:'t':_:_: xs) (C x y z) = let (z', xs') =  parseNo xs 0 in C x y z'
parseCommand _ _ = error "+"

parseNo :: String -> Int -> (Int, String)
parseNo [] a = (a, [])
parseNo (x:xs) a | isDigit x = parseNo xs (digitToInt  x + 10 * a)
                 | otherwise = (a, x:xs)

type Crate = Char

addCrate :: String -> String
addCrate ('[':c:']':_) = [c]
addCrate _ = ""

chunk :: Int -> String -> [String]
chunk _ [] = []
chunk n xs = take n xs : chunk n (drop n xs)


addCrates :: [String] -> [[Crate]] -> [[Crate]]
addCrates [] cs = cs
addCrates (x:xs) cs = addCrates xs updatedStacks
    where updatedStacks = [z ++ y | (y,z) <-  zip cs (map addCrate (chunk 4 x))]

createStacks :: String -> [[Crate]]
createStacks x = map (const []) (words x)

eval :: ([Crate] -> [Crate]) -> [Cmd] -> [[Crate]] -> [[Crate]]
eval f cmds css = foldr (exec f) css cmds

exec :: ([Crate] -> [Crate]) -> Cmd -> [[Crate]] -> [[Crate]]
exec f (C amount from to) css = updateN (to-1) (updateN (from-1) css newFrom) newTo
    where
          oldTo = css !! (to - 1)
          oldFrom = css !! (from - 1)
          newTo = f (take amount oldFrom) ++ oldTo
          newFrom = drop amount oldFrom

updateN :: Int -> [a] -> a -> [a]
updateN n xs x = let (b,a) = splitAt n xs in  b ++ [x] ++ drop 1 a

tops :: [[Crate]] -> String
tops = map (\x -> if not (null  x) then head x else ' ' )


--day6
day6 = do
    content <- readFile "input6.txt"
    let bundled = lines content
    print $ start 4 (head bundled) 1
    print $ start 14 (head bundled) 1
    return ()

start :: Int -> String -> Int -> Int
start amount str n | length (dedup $ take amount str) == amount = n + amount - 1
            | otherwise = start amount (drop 1 str) (n+1)

dedup :: Ord a => [a] -> [a]
dedup = map head . group . sort

--day7
day7 = do
    content <- readFile "input7.txt"
    let bundled = lines content
    let (tree, _) = readInput7' bundled
    let sizes = getSizes tree
    print $ sum $ filter (<=100000) $ toInts sizes []
    let (Dir _ _ size_total)= sizes
    let free = 70000000 - size_total
    let needed = 30000000 - free
    print $ minimum $ filter (>=needed) $ toInts sizes []
    return ()

data Tree a = Dir String [Tree a] a | File String a
    deriving Show

readInput7' :: [String] -> (Tree Int, String)
readInput7' = foldl readInput7 (Dir "/" [] 0,"/")

readInput7 ::  (Tree Int, String) -> String  -> (Tree Int, String)
readInput7 acc [] = acc
readInput7   (tree,state) line = let split = words line in
                                if head split == "$" then handleCmd (drop 1 split) (tree,state)
                                else addContained tree state (words line)

handleCmd :: [String] -> (Tree Int, String) -> (Tree Int, String)
handleCmd ("ls":_) x = x
handleCmd ("cd":"/":_) (tree,state) = (tree, "/")
handleCmd ("cd":"..":_) (tree,state) = (tree, reverse $ dropWhile (/= '/') $ drop 1 $ reverse state)
handleCmd ("cd":next:_) (tree,state) = (tree, state++next++"/")
handleCmd _ _ = error "eh"

addContained :: Tree Int -> String -> [String] -> (Tree Int,String)
addContained tree path line | head line == "dir" = (addDir tree path (line !! 1),path)
                            | otherwise          = (addFile tree line path,path)


addFile :: Tree Int -> [String] -> String -> Tree Int
addFile tree (size:name:_) path = let f = File name (read size) in addFile' f path tree
addFile _ _ _ = error "ah"

addDir :: Tree Int -> String -> String -> Tree Int
addDir tree path name = addFile' (Dir (path++name++"/") [] 0) path tree

addFile' :: Tree Int -> String -> Tree Int -> Tree Int
addFile' _ _ (File name size) = File name size
addFile' toAdd path (Dir name subDirs _)| name == path = Dir name (toAdd:subDirs) 0
                                        | otherwise    = Dir name (map (addFile' toAdd path) subDirs) 0

toSize :: Tree Int -> Int
toSize (File _ size) = size
toSize (Dir _ xs _) = sum $ map toSize xs

getSizes :: Tree Int -> Tree Int
getSizes (File name size) = File name size
getSizes (Dir name xs _) = Dir name (map getSizes xs) (sum $ map toSize xs)

toInts ::  Tree Int -> [Int] -> [Int]
toInts  (File _ size) ns = ns
toInts (Dir _ xs size) ns = size: foldr toInts ns xs

--day8
day8 = do
    content <- readFile "input8.txt"
    let bundled = lines content
    let (hori, verti,_,_) = parse8' bundled
    let dedup' = map head . group . sort
    print $ length $ dedup' $ concatMap visible (hori++verti++map reverse hori++map reverse verti)
    let all = dedup' $  concat (hori++verti)
    let table = map (\(BT (x,y) h) -> ((x,y), h) ) all
    print $ maximum (map (scenicScore table) all)

scenicScore :: [((HorI, VerI), Int)] -> BioTree -> Int
scenicScore table (BT (x,y) h) = dir1 * dir2 * dir3 * dir4
    where dir1 = getDir (\(x',y') -> (x'+1,y'))
          dir2 = getDir (\(x',y') -> (x'-1,y'))
          dir3 = getDir (\(x',y') -> (x',y'+1))
          dir4 = getDir (\(x',y') -> (x',y'-1))
          getDir f = viewable (BT (x,y) h) table f (x,y) 0

viewable :: BioTree -> [((HorI, VerI), Int)] -> ((Int,Int)->(Int,Int)) -> (Int,Int) -> Int ->  Int
viewable (BT (x,y) h) table f curr acc = case lookup (f curr) table of
                                            Nothing -> acc
                                            Just x -> if x<h then viewable (BT (x,y) h) table f (f curr) (acc+1)
                                                             else acc+1

visible :: TreeRow -> TreeRow
visible [] = []
visible (x:xs)= visible' x [x] xs

visible' :: BioTree ->  [BioTree] ->  [BioTree] -> [BioTree]
visible' _ acc [] =  acc
visible' (BT maxName maxHeight) acc ((BT name height):trees) =  if height > maxHeight then visible' (BT name height) (BT name height:acc) trees
                                                                                  else visible' (BT maxName maxHeight) acc trees

data BioTree = BT (HorI, VerI) Int
  deriving (Show, Eq, Ord)
type TreeRow = [BioTree]
type HorI = Int
type VerI = Int

parse8' :: [String] -> ([TreeRow], [TreeRow], HorI, VerI)
parse8' xs = foldl (\(hs,vs,hi,vi) str -> parse8 (hs,vs,hi+1,0) str) (empties, empties, -1,0)  xs
    where empties = replicate (length$ head xs) ([] :: TreeRow)

parse8 :: ([TreeRow], [TreeRow], HorI, VerI) -> String -> ([TreeRow], [TreeRow], HorI, VerI)
parse8  acc []  = acc
parse8 (hts, vts, hi, vi) (c:cs)  = parse8 (uhs,uvs,hi, vi+1) cs
  where uhs = zippy hi hts
        uvs = zippy vi vts
        zippy index tss = [ if i == index then newTree:x else x | (i,x) <- zip [0..] tss]
        newTree = BT (hi, vi) (read [c])
        
-- day9
day9 = do
    content <- readFile "input9.txt"
    let bundled = lines content
    let (_,_,journey) = foldl parse9 ((0,0), (0,0), []) bundled
    print $ length  $ dedup journey
    print $ length $ dedup $ last (take 9 $ iterate followHead journey)
    return ()

type TailPos = (Int,Int)
type HeadPos = (Int, Int)
type TailVisited = [TailPos]
data Direction = R|U|L|D

parse9 :: (TailPos, HeadPos, TailVisited) -> String ->  (TailPos, HeadPos,TailVisited)
parse9 acc ('R':no)  = move9 R (read no) acc
parse9 acc ('U':no)  = move9 U (read no) acc
parse9 acc ('L':no)  = move9 L (read no) acc
parse9 acc ('D':no)  = move9 D (read no) acc
parse9 _ _ = error "9"

move9 :: Direction -> Int -> (TailPos, HeadPos, TailVisited) -> (TailPos, HeadPos,TailVisited)
move9 _ 0 acc = acc
move9 R n (t, (hx,hy), tps) = let tpos = move9' (hx,hy) (hx+1, hy) t in move9 R (n-1) (tpos, (hx+1, hy), tpos:tps)
move9 U n (t, (hx,hy), tps) = let tpos = move9' (hx,hy) (hx, hy+1) t in move9 U (n-1) (tpos, (hx, hy+1), tpos:tps)
move9 L n (t, (hx,hy), tps) = let tpos = move9' (hx,hy) (hx-1, hy) t in move9 L (n-1) (tpos, (hx-1, hy), tpos:tps)
move9 D n (t, (hx,hy), tps) = let tpos = move9' (hx,hy) (hx, hy-1) t in move9 D (n-1) (tpos, (hx, hy-1), tpos:tps)

move9' :: HeadPos -> HeadPos -> TailPos -> TailPos
move9' (oldhx,oldhy) (hx,hy) (tx,ty) = if shouldMove (hx, hy) (tx,ty) then (oldhx,oldhy) else (tx,ty)

move9'' :: HeadPos -> HeadPos -> TailPos -> TailPos
move9'' (oldhx,oldhy) (hx,hy) (tx,ty) = if shouldMove (hx, hy) (tx,ty) then diagOption else (tx,ty)
  where diagOption =  if diag (oldhx,oldhy) (hx,hy) then dm else (oldhx,oldhy)
        dm | hx == tx && hy>ty = (tx, ty+1)
           | hx == tx && hy<ty = (tx, ty-1)
           | hy ==ty && hx>tx  = (tx+1, ty)
           | hy ==ty && hx<tx  = (tx-1, ty)
           | oldhx-1 == hx && oldhy-1 == hy = (tx-1, ty-1)
           | oldhx-1 == hx && oldhy+1 == hy = (tx-1, ty+1)
           | oldhx+1 == hx && oldhy-1 == hy = (tx+1, ty-1)
           | oldhx+1 == hx && oldhy+1 == hy = (tx+1, ty+1)
           | otherwise = error "91"
 
diag :: (Eq a) => (a, a) -> (a, a) -> Bool
diag (oldhx,oldhy) (hx,hy) = oldhx /= hx && oldhy /= hy

followHead :: [HeadPos] -> TailVisited
followHead poss = let prevnexts = zip ((0,0):reverse poss) (reverse poss) in foldl foldFn [] prevnexts
  where foldFn list (prev,next) =  move9'' prev next (if null list then (0,0) else head list):list

shouldMove :: HeadPos -> TailPos -> Bool
shouldMove (hx, hy) (tx,ty) = hx == tx+2 || hy == ty+2 || hx == tx-2 || hy == ty-2
