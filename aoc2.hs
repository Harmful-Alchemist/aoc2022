day10 = do
    content <- readFile "input10.txt"
    let bundled = lines content
    let (S _ _ x xs) = foldl exec10 (S 1 1 [] []) $ parse10 bundled []
    print $ sum x
    print $ map reverse $ group40 xs []
    putStrLn $ unlines $ map (draw10 0 "") $ map reverse $ group40 xs []
    return ()

data Op a = Noop | AddXStart a | AddXEnd a
    deriving Show

parse10 :: [String] -> [Op Int] -> [Op Int]
parse10 [] acc = reverse  acc
parse10 (x:xs) acc  | x == "noop" = parse10 xs (Noop : acc)
                    | otherwise   = parse10 xs (AddXEnd n : AddXStart n :acc)
                        where n = read $ words x !! 1

type Cycle = Int
type RegisterX = Int
type SumSignal = [Int]
type RegisterXHistory = [RegisterX]
data State a b c d = S a b c d
    deriving Show

inCycle :: Int -> Bool
inCycle n =  n `elem` take (n `div` 20) (20 : [60,100..])

updateSignal :: Cycle -> RegisterX -> SumSignal -> SumSignal
updateSignal cycle x signal = if inCycle cycle then (x*cycle):signal else signal

exec10 :: State Cycle RegisterX SumSignal RegisterXHistory -> Op Int -> State Cycle RegisterX SumSignal RegisterXHistory
exec10 (S cycle x s xs) op = let sumSignal = updateSignal cycle x s in case op of
    AddXEnd n -> S (cycle + 1) (x+n) sumSignal (x:xs)
    _         -> S (cycle + 1) x sumSignal (x:xs)

group40 :: RegisterXHistory ->  [RegisterXHistory] -> [RegisterXHistory]
group40 [] acc = acc
group40 xs acc = group40 (drop 40 xs) (take 40 xs : acc)

draw10 :: Int -> String -> RegisterXHistory -> String
draw10 _ acc []  = reverse acc
draw10 turn acc (x:xs) = draw10 (turn+1) new xs
    where new = if hitsSprite then '#':acc else '.':acc
          hitsSprite = x == turn || x == turn+1 || x == turn-1