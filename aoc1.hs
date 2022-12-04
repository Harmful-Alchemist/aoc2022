import Data.List (intersect, sort, elemIndex, isSubsequenceOf)
import Data.Char (ord, isUpper, digitToInt)
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


