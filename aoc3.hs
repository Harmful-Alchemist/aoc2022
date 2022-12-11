import Data.List
day11 = do
    content <- readFile "input11.txt"
    let bundled = lines content
    let monkeys =  parse11 bundled []
    let (x:y:_) =  take 2 $ reverse $ sort $ map monkeyToScore $ last $ take 21 $ iterate (round' (`div` 3) ) monkeys
    print (x*y)
    let (x':y':_) =  take 2 $ reverse $ sort $ map monkeyToScore $ last $ take 10001 $ iterate (round' id ) monkeys
    print (x'*y')
    return ()

type Item = Integer
type Operation = (Integer-> Integer)
type Test = Integer
type MonkeyIndex = Int
type ItemsInspected = Int
data Monkey = M [Item] Operation Test MonkeyIndex MonkeyIndex ItemsInspected

instance Show Monkey where
    show (M items _ _ to to' _) = "Monkey " ++ show items ++ " true to: " ++ show to ++ " false to: " ++ show to'

defaultMonkey :: Monkey
defaultMonkey= M [] (const 0) 0 0 0 0

parse11 :: [String] -> [Monkey] -> [Monkey]
parse11 [] acc = reverse acc
parse11 ("":ls) acc = parse11 ls acc
parse11 (l:ls) acc | head (words l) == "Starting" = let (M i o t mit mif ii) = head acc in updateMonkey (M (itemsParse (words l)) o t mit mif ii) ls acc
 | head (words l) == "Operation:" = let (M i o t mit mif ii) = head acc in updateMonkey (M i (opParse (words l)) t mit mif ii) ls acc
 | head (words l) == "Test:" = let (M i o t mit mif ii) = head acc in updateMonkey (M i o (testParse (words l)) mit mif ii) ls acc
 | head (words l) == "If" && words l !! 1 == "true:" = let (M i o t mit mif ii) = head acc in updateMonkey (M i o t (indexParse (words l)) mif ii) ls acc
 | head (words l) == "If" && words l !! 1 == "false:" = let (M i o t mit mif ii) = head acc in updateMonkey (M i o t mit (indexParse (words l)) ii) ls acc
 | head (words l) == "Monkey" = parse11 ls (defaultMonkey:acc)
parse11 rem _ = error $ unlines rem


updateMonkey :: Monkey -> [String] -> [Monkey] -> [Monkey]
updateMonkey monkey ls acc = parse11 ls (monkey:drop 1 acc)

itemsParse :: [String] -> [Item]
itemsParse items = map (read . removeComma) (drop 2 items)
    where removeComma string = if last string == ',' then reverse $ drop 1 $ reverse string else string

opParse :: [String] -> Operation
opParse (_:_:_:_:"+":"old":_) = \n -> n+n
opParse (_:_:_:_:"*":"old":_) = \n -> n*n
opParse (_:_:_:_:"+":m:_) = (+read m)
opParse (_:_:_:_:"*":m:_) = (* read m)
opParse _ = error "11.1"

testParse :: [String] -> Test
testParse words = read (last words)

indexParse :: [String] -> Int
indexParse words = read (last words)


round' :: (Integer->Integer) ->  [Monkey] -> [Monkey]
round' f monkeys = roundKeepAway f 0 (monkeySafe monkeys) monkeys
roundKeepAway :: (Integer->Integer) ->  Int -> Test -> [Monkey] -> [Monkey]
roundKeepAway f index safe monkeys  | index == length  monkeys = monkeys
                    | otherwise                           = roundKeepAway f (index + 1) safe (handleMonkey f safe (monkeys !! index) index monkeys) 

handleMonkey :: (Integer->Integer) -> Test -> Monkey -> Int -> [Monkey] -> [Monkey]
handleMonkey _ _ (M [] _ _ _ _ _ ) _ acc = acc
handleMonkey f safe (M (i:is) op t ti fi ii) ownIndex acc = handleMonkey f safe  monkey ownIndex updated
    where monkey = M is op t ti fi (ii+1)
          newWorryLevel = let x = f $ op i in  x `rem` safe
          updateIndex = if newWorryLevel `mod` t == 0 then ti else fi
          updatedMonkey = let (M i o t mit mif ii) = acc !! updateIndex in M (i++[newWorryLevel]) o t mit mif ii
          updated = [if i == ownIndex then monkey else if i == updateIndex then updatedMonkey else m | (m,i) <- zip acc [0..] ]

monkeyToScore :: Monkey -> Int
monkeyToScore (M _ _ _ _ _ ii) = ii

monkeySafe :: [Monkey] -> Test
monkeySafe ms = product $ map (\(M _ _ t _ _ _) -> t) ms