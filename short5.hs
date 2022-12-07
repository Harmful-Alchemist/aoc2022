import Data.Char
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