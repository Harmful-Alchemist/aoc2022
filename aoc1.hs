import Data.List (intersect, sort, elemIndex, isSubsequenceOf, group)
import Data.Char (ord, isUpper, digitToInt, isDigit)
import Data.Maybe (fromJust)
import Control.Exception (handle)

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

valueItem :: [Char] -> Int
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
toInt x [] = x
toInt x (y:ys) = toInt (10*x + digitToInt y) ys

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