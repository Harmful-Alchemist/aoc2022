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