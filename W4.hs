module W4 where

import Control.Monad
import Data.List
import Data.IORef
import System.IO

-- Week 4:
--   * The IO type
--   * do-notation
--
-- Useful functions / operations:
--   * putStrLn
--   * getLine
--   * readLn
--   * replicateM
--   * readFile
--   * lines
--
-- NB! Do not add any additional imports!
--
-- NB! Do not use IORef in any exercise except 15!

------------------------------------------------------------------------------
-- Ex 1: define an IO operation hello that prints two lines. The
-- first line should be HELLO and the second one WORLD

hello :: IO ()
--hello = undefined

hello = do 
   putStrLn "HELLO"
   putStrLn $ "WORLD"

------------------------------------------------------------------------------
-- Ex 2: define the IO operation greet that takes a name as an
-- argument and prints a line "HELLO name".

greet :: String -> IO ()
--greet name = undefined

greet name = do
   putStrLn $ "HELLO " ++ name
   
------------------------------------------------------------------------------
-- Ex 3: define the IO operation greet2 that reads a name from the
-- keyboard and then greets that name like the in the previous
-- exercise.
--
-- Try to use the greet operation in your solution.

greet2 :: IO ()
--greet2 = undefined

greet2 = do
   --putStrLn "Write something!"
   s <- getLine
   putStrLn $ "HELLO "++s

------------------------------------------------------------------------------
-- Ex 4: define the IO operation getSum that reads two numbers, on
-- separate lines, from the user, and produces their sum.
--
-- Remember the operation readLn.

getSum :: IO Int
--getSum = undefined

getSum = do
    i <- readLn
    j <- readLn
    return $ i+j
    
------------------------------------------------------------------------------
-- Ex 5: define the IO operation readWords n which reads n lines from
-- the user and returns them in alphabetical order.

readWords :: Int -> IO [String]
--readWords n = undefined

readWords n = do 
    if (n > 0) 
        then do 
          word <- getLine
          words <- readWords (n-1)
          return $ sort(word:words)
        else return []

------------------------------------------------------------------------------
-- Ex 6: define the IO operation readUntil f, which reads lines from
-- the user and returns them as a list. Reading is stopped when f
-- returns True for a line. (The value for which f returns True is not
-- returned.)
--
-- You'll probably need to make readUntil recursive (or use a
-- recursive helper operation).

readUntil :: (String -> Bool) -> IO [String]
--readUntil f = undefined

readUntil f = do 
  --rest = dropWhile (/=c) xs
  --putStr $ f
  word <- getLine
  if f word 
    then
      --word <- getLine
      return []
    else do
      xs <- readUntil f
      --print word
      --word : xs
      return $ word:xs
--    s <- readLn
--    print (sum s)
--    isums (n-1)
--    return $ sum s
--readUntil f = do word <- getLine
 --                 if f word
 -- 	                  then return []
 -- 	                  else do words <- readUntil f
--                        return $ word:words

--readUntil f = do word <- getLine
--                 if f word
--                   then return []
--                   else do words <- readUntil f
--                     return word:words

--  		return $ word:words
--   let words = ["yksi", "kaksi"]
--   return $ words
--readUntil f = do 
--	word <- getLine
--      if f word
--        then 
--          words <- readUntil f
--          return $ word:words
--        else return []
--readUntil f = do word <- getLine    
                 --if (f == false)-- word
                 --  then  
                 --    words <- readUntil f
  --               let words = ["yksi", "kaksi"]
    --             return $ word:words   --else 
                     --words <- ["yksi", "kaksi"]      
                 --else --words <- readUntil f
                 --  return []--return $ word:words  
stringIsEven :: String -> Bool
stringIsEven = even . length 
------------------------------------------------------------------------------
-- Ex 7: isums n should read n numbers from the user and return their
-- sum. Additionally, after each read number, the sum up to that
-- number should be printed.
--
-- Reminder: do not use IORef

isums :: Int -> IO Int
--isums n = undefined
isums n = summ n 0
  where
    summ 0 endsum = return endsum
    summ m endsum = do
        x <- readLn
        let x' = endsum + x
        print x'
        summ (m - 1) x'
--readAndSum :: Int -> IO Int
--readAndSum 0 = return 0
--readAndSum n = do
--  i <- readLn
--  s <- readAndSum (n-1)
--  return $ i+s

--sumtogether :: Int -> Int -> Int
--sumtogether a b
--  return sum $ a b

--isums n = do
--isums n = fmap sum (replicateM n readLn)--do
    --numbers <- replicateM n readLn
    --return (sum numbers)
    --isums n = fmap sum (replicateM n readLn)
  --let summ = 0
  --numbers <- replicateM n readLn
  --print (numbers)
  --summ = summ+numbers
  --print summ
  --return $ sum numbers
  --return $ sum numbers
--isums 0 = return 0
--isums n = do 
--  while n > 0 do 
--    s <- readLn
--    print (sum s)
--    isums (n-1)
--    return $ sum s
  --s <- readLn
--  if n > 0 then do
--    putStrLn "Write something! "
--    s <- readLn
--    let a = (n+s)
--    print a
    --putStrLn $ show (a)
    --putStrLn $ show (s+n)
--    isums (n-1)
--    return $ n+s
--  else
--    return $ 0
--isums 0 = return sum
--isums n = go where
--  go = do


--isums n = go 0 n
--  where go sum 0 = return sum
--        go sum n = do i <- readLn
--                      let sum' = sum+i
--                      print sum'
--                      go sum' (n-1)

--test1 :: Int
--test1 = while (< 1000) (* 2) 2

readAndSum :: Int -> IO Int
--readAndSum 0 = return 0
--readAndSum n = do
--  i <- readLn
--  s <- readAndSum (n-1)
--  return $ i+s

readAndSum n = do
  --let summ = 0
  numbers <- replicateM n readLn
  print (numbers)
  --summ = summ+numbers
  --print summ
  --return $ sum numbers
  return $ sum numbers

printDescription :: Int -> IO ()
printDescription n
  | even n    = putStrLn "even"
  | n==3      = putStrLn "three"
  | otherwise = print n

myHead :: [Int] -> Int
myHead xs = case xs of (x:_) -> x
                       []    -> -1

query :: IO ()
query = do
  putStrLn "Write something!"
  --s <- getLine
  s <- readLn
  --let n = length s
  --putStrLn $ "You wrote "++show n++" characters: "++s
  putStrLn $ "You wrote "++show s++" characters: "++s

returningQuery :: String -> IO String
returningQuery question = do
  putStrLn question
  getLine

produceThree :: IO Int
produceThree = return 3

printThree :: IO ()
printThree = do
  three <- produceThree
  putStrLn $ show three

--isums 0 = return
--isums n = do 
--let n2 = 0
    --while (n > 0) do
        --then do 
    --      number <- readLn
    --      numbers <- numbers + number
    --      putStrLn $ ""++numbers
    --      n = (n - 1)

   -- let loop 0 = return ()
      -- loop n = do
     --     putStrLn "x not yet 0, enter an adjustment"
      --    a <- readLn
      --    isums (n+a)
          --loop 23
      --    putStrLn "x reached 0! Exiting"
      --    return $ a
        --else return []

------------------------------------------------------------------------------
-- Ex 8: when is a useful function, but its first argument has type
-- Bool. Write a function that behaves similarly but the first
-- argument has type IO Bool.

whenM :: IO Bool -> IO () -> IO ()
--whenM cond op = undefined

whenM cond op = do c <- cond
                   when c op
--whenM cond op = do tru <- cond
--                    when tru op


------------------------------------------------------------------------------
-- Ex 9: implement the while loop. while condition operation should
-- run operation as long as condition returns True.
--
-- Examples:
-- while (return False) (putStrLn "IMPOSSIBLE")  -- prints nothing
--
-- let ask :: IO Bool
--     ask = do putStrLn "Y/N?"
--              line <- getLine
--              return $ line == "Y"
-- in while ask (putStrLn "YAY!")
--
-- This prints YAY! as long as the user keeps answering Y

while :: IO Bool -> IO () -> IO ()
--while cond op = undefined

--while cond op = do c <- cond
--                   while c do op
while cond op = whenM cond $ do op
                                while cond op

--while cond op = 


--while haskell
fibonacci :: Integer -> Integer
fibonacci n = fibonacci' 0 1 n

fibonacci' :: Integer -> Integer -> Integer -> Integer
fibonacci' a b 1 = b
fibonacci' a b n = fibonacci' b (a+b) (n-1)



------------------------------------------------------------------------------
-- Ex 10: given a string and an IO operation, print the string, run
-- the IO operation, print the string again, and finally return what
-- the operation returned.
--
-- Note! the operation should be run only once
--
-- Examples:
--   debug "CIAO" (return 3)
--     - prints two lines that contain CIAO
--     - returns the value 3
--   debug "BOOM" getLine
--     1. prints "BOOM"
--     2. reads a line from the user
--     3. prints "BOOM"
--     4. returns the line read from the user

debug :: String -> IO a -> IO a
--debug s op = undefined

debug s op = do 
  putStrLn s
  --print s
  --ss <- getLine
  ss <- op
  putStrLn s --print s
  return ss 
------------------------------------------------------------------------------
-- Ex 11: Reimplement mapM_ (specialized to the IO type) using
-- recursion and pattern matching.
--
-- In case you don't know what mapM_ does, it takes a parameterized IO
-- operation and a list of parameters, and runs the operation for each
-- value in the list.
--
-- Remember to use `return ()` so that you get the type right!

mymapM_ :: (a -> IO b) -> [a] -> IO ()
--mymapM_ = undefined

mymapM_ _ [] = return ()
mymapM_ f (x:xs) = f x >> mymapM_ f xs
--mymapM_ a b c

--mymapM_ f [] = return ()
--mymapM_ f (x:xs) = do f x
--                      mymapM_ f xs


------------------------------------------------------------------------------
-- Ex 12: Reimplement the function forM using pattern matching and
-- recursion.

myforM :: [a] -> (a -> IO b) -> IO [b]
--myforM as f = undefined

myforM [] f = return []
myforM (a:as) f = do
  x <- f a
  xs <- myforM as f
  return $ x : xs
--myforM [] f = return ()
--myforM (x:xs) f =  
------------------------------------------------------------------------------
-- Ex 13: sometimes one bumps into IO operations that return IO
-- operations. For instance the type IO (IO Int) means an IO operation
-- that returns an IO operation that returns an Int.
--
-- Implement the function doubleCall which takes an operation op and
--   1. runs op
--   2. runs the operation returned by op
--   3. returns the value returned by this operation
--
-- Examples:
--   - doubleCall (return (return 3)) is the same as return 3
--
--   - let op :: IO (IO [String])
--         op = do l <- readLn
--                 return $ replicateM l getLine
--     in doubleCall op
--
--     works just like
--
--     do l <- readLn
--        replicateM l getLine

doubleCall :: IO (IO a) -> IO a
--doubleCall op = undefined

doubleCall op = do 
  x <- op
  x

------------------------------------------------------------------------------
-- Ex 14: implement the analogue of function composition (the (.)
-- operator) for IO operations. That is, take an operation op1 of type
--     a -> IO b
-- an operation op2 of type
--     c -> IO a
-- and a value of type
--     c
-- and returns an operation op3 of type
--     IO b
--
-- op3 should of course
--   1. take the value of type c and pass it to op2
--   2. take the resulting value (of type a) and pass it to op1
--   3. return the result (of type b)

compose :: (a -> IO b) -> (c -> IO a) -> c -> IO b
--compose op1 op2 c = undefined

compose op1 op2 c = do 
  x <- op2 c
  op1 x  

--x <- a:op1

--y <- b:op2
  
--myforM (a:as) f = do
--  x <- f a
--  xs <- myforM as f
--  return $ x : xs

------------------------------------------------------------------------------
-- Ex 15: This exercises is about IORefs and operations that return
-- operations.
--
-- Implement the function mkCounter that returns the io operations
-- inc :: IO () and get :: IO Int. These operations should work like this:
--
--   get returns the number of times inc has been called
--
-- In other words, a simple stateful counter.
--
-- An example of how mkCounter works in GHCi:
--
--  *W4> (inc,get) <- mkCounter
--  *W4> inc
--  *W4> inc
--  *W4> get
--  2
--  *W4> inc
--  *W4> inc
--  *W4> get
--  4

mkCounter :: IO (IO (), IO Int)
--mkCounter = undefined

mkCounter = do 
  re <- newIORef (0 :: Int)
  let r2 = readIORef re
  --r2 <- newIORef 1
  let r = modifyIORef re (+1) --xs
  return (r,r2)
  --readIORef r
  --readIORef r2 

sumList :: [Int] -> IO Int
sumList xs = do r <- newIORef 0
                mapM_ (\x -> modifyIORef r (x+)) xs
                readIORef r
------------------------------------------------------------------------------
-- Ex 16: fetch from the given file (Handle) the lines with the given
-- indices. Line indexing starts from 1. You can assume that the
-- numbers are given in ascending order.
--
-- Have a look at the docs for the System.IO module for help.

hFetchLines :: Handle -> [Int] -> IO [String]
--hFetchLines h nums = undefined

--hFetchLines h nums = do
--  handle <- openFile h ReadMode
--  contents <- hGetContents handle
  --line <- hGetLine h
  --lines <- getLinesSeq h
--  let ee = lines contents
--  print ee
  --putStr(ee)
  --putStr (take nums contents)
  --hClose h
--  let dd = ["kaksi","kolme"]
--  print dd--show(dd)
--  return $ dd

  ------------------------------
hFetchLines h nums = do cont <- hGetContents h
                        let split = lines cont
                        return $ pick 1 nums split
  where pick _ []       _         = []
        pick _ _        []        = []
        pick i (n:nums) (s:split)
          | i==n      = s:pick (i+1) nums split
          | otherwise = pick (i+1) (n:nums) split
  ------------------------------
--------------------------
--f :: [a] -> [int] -> [a]
--f lst = map fst $ filter (odd.snd) indexed where
--f lst numb = map fst $ filter (numb.snd) indexed where
--    indexed = zip lst [0..]

--hFetchLines h nums = do
--  hand <- openFile h ReadMode

  --hClose hand
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Ex 17: CSV is a file format that stores a two-dimensional array of
-- values in a file. Each row of the file is a row of the array. Each
-- row of the file consists of values on that row separated with the ,
-- character.
--
-- Implement the function readCSV that reads a CSV file and returns it
-- as a list of lists.
--
-- NB! You don't need to handle the intricacies of real CSV, e.g.
-- quoting. You can assume each , character starts a new field.
--
-- NB! The lines might have different numbers of elements.

readCSV :: FilePath -> IO [[String]]
--readCSV path = undefined

--readCSV path = do
--  res <- readCSV path
--  return $ res
readCSV path = do str <- readFile path
                  --return $ map process $ lines str
                  --let f = (==',')
                  return $ map splitWith $ lines str
     --where process xs = case break (==',') xs of (a,[])    -> [a] 
     --                                            (a,',':b) -> a:process b

--split :: Char -> [String] -> [[String]]
--split c [] = []
--split c xs = start : split c (drop 1 rest)
--  where start = takeWhile (/=c) xs
--        rest = dropWhile (/=c) xs

            -- splitWith :: Eq a => (a -> Bool) -> [a] -> [[a]]
      --where splitWith (==',') list = case splitWith' (==',') list of
      where splitWith list = case splitWith2 (==',') list of  
              []:result -> result
              result -> result
              --putStrLn f
              --print list

splitWith2 :: Eq a => (a -> Bool) -> [a] -> [[a]]
splitWith2 _ [] = []
splitWith2 f (a:[]) = if f a then [] else [[a]]--[[a]]
splitWith2 f (a:b:tail) =
  let next = splitWith2 f (b : tail)
  in if f a
    then if a == b
      then next
      else [] : next
    else case next of
      [] -> [[a]]
      nextHead:nextTail -> (a : nextHead) : nextTail
  
--readCSV path = do str <- readFile path
--                  let vir = (',')
--                  let str2 = splitWith vir str
--                  return str
 
                  --return $ map words . lines $ str2
                  --return $ map (splitWith (== ',') str) . lines --$ str

                  --where process [] = False
                  --  match (c':_) = c==c'
                  --  process x = take (k+1) x

                  --return $ map words . lines $ str
                  --return $ map words . drop 1 . lines $ str

  --readCSV path = do
  -- csvdata <- readFile path
  -- return $ map . lines csvdata
   --return $ (map (map read . words) . lines) mdata :: [[String]]
  --(map (map read . words) . lines) mdata :: [[Int]]
------------------------------------------------------------------------------
------------------------------------------------------------------------------
-- Ex 18: your task is to compare two files, a and b. The files should
-- have the same contents, but if lines at index i differ from each
-- other, you should print
--
-- < file a version of the line
-- > file b version of the line
--
-- Example:
--
-- File a contents:
-- a
-- aa
-- x
-- aa
-- bb
-- cc
--
-- File b contents:
-- a
-- aa
-- bb
-- aa
-- cc
-- dd
--
-- Output:
-- < x
-- > bb
-- < bb
-- > cc
-- < cc
-- > dd
--
-- NB! You can assume the files have the same number of rows.
--
-- Hint! It's probably wise to implement a pure function for finding
-- the differing lines. A suitable type could be
-- [String] -> [String] -> [String].
--compareBB :: [String] -> [String] -> [String]

compareFiles :: FilePath -> FilePath -> IO ()
--compareFiles a b = undefined

--compareFiles a b = do ac <- readFile a
--                      bc <- readFile b
--                      mapM_ putStrLn $ compareHelper (lines ac) (lines bc)

--compareHelper []     []     = []
--compareHelper (a:as) (b:bs)
--  | a /= b    = ("< "++a):("> "++b):compareHelper as bs
--  | otherwise = compareHelper as bs

compareFiles a b = do  
    inh <- openFile a ReadMode
    outh <- openFile b ReadMode
    --inh <- readFile a --ReadMode
    --outh <- readFile b --ReadMode
    fin <- hGetContents inh
    fin2 <- hGetContents outh
    
--    putStrLn "File a contents:"
--    putStrLn fin
--    putStrLn ""
--    putStrLn "File b contents:"
--    putStrLn fin2
--    putStrLn ""
--    putStrLn "Output:"
    --putStrLn "Linjat:"
  --  compareBB fin fin2
    --let ii = length $ filter (== EQ) $ map (uncurry compare) $ zip fin fin2
    --let i2 = filter (== EQ) $ map (uncurry compare) $ zip fin fin2
    --print i2
    --inhA <- openFile a ReadMode
    --outhA <- openFile b ReadMode
    --let csA = length $ lines fin
    --print csA
    --let csB = length (lines fin2) 
    --print csB
    --let k2k = csA `intersect` csB
    --myElems [] fin2 = True
    --let kkk = myElems (x:fin) fin2 = if myElem x fin2 then myElems fin fin2 else False
    --print $ fin ++ fin2
    mapM putStrLn $ compareBB (lines fin) (lines fin2)
    
    hClose inh
    hClose outh

compareBB []           []        = []
compareBB (a:astring) (b:bstring) 
  | a /= b = ("< "++a):("> "++b):compareBB astring bstring
  | otherwise = compareBB astring bstring

-----------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-- Ex 19: In this exercise we see how a program can be split into a
-- pure part that does all of the work, and a simple IO wrapper that
-- drives the pure logic.
--
-- Implement the function interact' that takes a pure function f of
-- type
--   (String, st) -> (Bool, String, st)
-- and a starting state of type st and returns an IO operation of type
-- IO st
--
-- interact' should read a line from the user, feed the line and the
-- current state to f. f then returns a boolean, a string to print and
-- a new state. The string is printed, and if the boolean is True, we
-- continue running with the new state. If the boolean is False, the
-- execution has ended and the state should be returned.
--
-- Example:
--
-- let f :: (String,Integer) -> (Bool,String,Integer)
--     f ("inc",n)   = (True,"",n+1)
--     f ("print",n) = (True,show n,n)
--     f ("quit",n)  = (False,"bye bye",n)
-- in interact' f 0
--

interact' :: ((String,st) -> (Bool,String,st)) -> st -> IO st
--interact' f state = undefined

interact' f state = do
  userinput <- getLine
  case f (userinput,state) of
    (True,  showstring, state2) ->
      do putStr showstring
         interact' f state2
    (False, showstring, state2) ->
      do putStr showstring
         return state2
--case expression of
--  pattern -> expression
--  pattern -> expression

--myHead :: [Int] -> Int
--myHead xs = case xs of (x:_) -> x
--                       []    -> -1

--choice :: IO a -> IO a -> IO a
--choice a b =
--  do putStr "a or b? "
--     x <- getLine
--     case x of "a" -> a
--               "b" -> b
--               _ -> do putStrLn "Wrong!"
--                       choice a b