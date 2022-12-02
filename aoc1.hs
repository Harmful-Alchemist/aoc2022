import Data.List
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
