module W7 where
{-}
-- Final week!
--
-- Think of this as the exam. You must get 4/10 exercises to pass the course.
--
-- Good luck.
--
-- NB. Do not add any imports!
-}
import Data.List
import Control.Monad
import Control.Monad.Trans.State

------------------------------------------------------------------------------
-- Ex 1: Count how many numbers in the input list are in the given
-- low-high range (inclusive)
--
-- Examples:
--   countRange 5 8 [] ==> 0
--   countRange 1 3 [1,2,3,4,5] ==> 3

countRange :: Int -> Int -> [Int] -> Int
--countRange low high is = undefined

countRange low high is = length(filter (\i -> i >= low && i <= high) is)

--countOfElem elem = length . filter (==elem)list
--countRange low high is = 
--count :: (a -> Bool) -> [a] -> Int
--count p xs = length $ filter p xs
--add :: Int -> State Int ()
--add i = do old <- get
--           put (old+i)
---------------------------------
{-
---------------------------------------------
-- Ex 2: Build a string that looks like an n*m chessboard:
--
--   #.#.#.#.
--   .#.#.#.#
--   #.#.#.#.
--
-- Examples:
--   chess 1 1 ==> "#\n"
--   chess 3 5 ==> "#.#.#\n.#.#.\n#.#.#\n"
--
-- Hint: it's easier to see how the chess board looks like if you run
--   putStr (chess 3 5)
-- in GHCi

chess :: Int -> Int -> String
--chess = undefined

chess 1 1 = "#\n"
--chess a b = "#.#.#\n.#.#.\n#.#.#\n" 

--putStr (chess 3 5)
chess a b = do
              if(counteven a == 0) then '#':[]
              else '.':[]  
--counteven :: [Integer] -> Integer
counteven :: Int -> Int
--counteven _ = 0
counteven x
  | x `mod` 2 == 0 = 0 -- NOTE: `mod` not `div`!
  | otherwise      = 1

----------------------
---------------------

rectangle2 :: Int -> Int -> String
rectangle2 h w = unlines (replicate h (replicate w '#'))

--rectangle2A :: Int -> String
--rect1 <- rectangle2A in1 = replicate in1 '#'
--rectangle2B :: Int -> String
--rect2 <- rectangle2B in2 = replicate in2 '.'

--rectangle3 :: String -> String -> String
--rectangle3 (rectangle2A in1) (rectangle2B in2) = map (in1 in2) 

evens [] = []
evens (x:xs) = odds xs

odds [] = []
odds (x:xs) = x:evens xs

allEqual [] = True
allEqual (x:xs) = all (==x) xs




strOdd :: Int -> Int -> String
strOdd a b = "#"++"."
--strEven :: String 
--strEven a b c = a*(c++b)[]

--fm :: Int -> Int -> String
--fm (h, w) p =  [ [if (y, x) == p then 1 else 0 | x <- [1..w]]
--                                               | y <- [1..h]]

replicate2 :: Int -> a -> [a]                
replicate2 n x = if (n <= 0) then [] else (x : replicate (n-1) x) 

underline :: String -> String
underline = flip replicate '-' . length
--------------------
-- Ex 5: define the function eeny that returns "eeny" for even inputs
-- and "meeny" for odd inputs.
--
-- Ps. have a look at the built in function "even"

eeny :: Integer -> String
eeny i = if even i then "eeny" else "meeny"
------------------
--pyramid 3 ==> "0,1,2,3,2,1,0"
pyramidA :: Integer -> String
pyramidA n = helper 0 n

helper k 0 = show k
helper k n = show k ++ "," ++ helper (k+1) (n-1) ++ "," ++ show k
-----------------

--pyramid :: Int -> String
--pyramid n = unlines $ p n
--  where p 1 = ["#"]
        --p n = map ('#':) (p (n-1)) ++ [replicate (2*n-1) '*']
--        p n = map ('#':) (p (n-1)) ++ [replicate (n) '#']

-}
------------------------------------------------------------------------------
-- Ex 3: Implement the function palindromify that chops a character
-- off the front _and_ back of a string until the result is a
-- palindrome.
--
-- Examples:
--   palindromify "ab" ==> ""
--   palindromify "aaay" ==> "aa"
--   palindromify "xabbay" ==> "abba"
--   palindromify "abracacabra" ==> "acaca"

palindromify :: String -> String
--palindromify = undefined

palindromify xs = if palindrome xs then xs
                  else palindromify (firstLast xs) 
-------------------------
firstLast::[a]->[a]
firstLast xs@(_:_) = tail (init xs); firstLast _ = []
-------------------------
palindrome :: String -> Bool
palindrome s = reverse s == s
  ------------------------------
-------------------------------
------------------------------------------------------------------------------
-- Ex 4: Remove all repetitions of elements in a list. That is, if an
-- element occurs in the input list 2 or more times in a row, replace
-- this with 1 occurrence.
--
-- DO NOT use any library list functions like head, tail, (++) and so on.
-- USE ONLY recursion and pattern matching to process the list.
--
-- It's ok to use (==) or compare obviously. If-then-else and guards
-- are fine too as long as you pattern match the list.
--
-- Examples:
--   unrepeat [True,True,True,True] => [True]
--   unrepeat [1,1,2,1,3,3,3] => [1,2,1,3]

unrepeat :: Eq a => [a] -> [a]
--unrepeat = undefined

--unrepeat :: [Int] -> [Int]
unrepeat [] = []
unrepeat [x] = [x]
unrepeat (x1:x2:xs)
  | x1==x2 = unrepeat (x2:xs)
  | otherwise = x1:unrepeat (x2:xs)

----------------------
----------------------
remdups :: [Int] -> [Int]
remdups [] = []
remdups [x] = [x]
remdups (x1:x2:xs)
  | x1==x2 = remdups (x2:xs)
  | otherwise = x1:remdups (x2:xs)

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates = rdHelper []
    where rdHelper seen [] = seen
          rdHelper seen (x:xs)
              | x `elem` seen = rdHelper seen xs
              | otherwise = rdHelper (seen ++ [x]) xs
--rmdups' :: (Eq a) => [a] -> [a]
--rmdups' [] = []
--rmdups' [x] = [x]
--rmdups' (x:xs) = x : [ k  | k <- rmdups'(xs), k /=x ]
--rmdups' (x:xs) = x : [ k  | k <- rmdups'(xs), k /= xs!!1 ]
--rmdups' (x:xs) = x : [ k  | k <- rmdups'(xs!!1), k /= xs]
--rmdups' (x:xs) = if (x == xs!!1) then rmdups xs
--                 else x
--[1,2,3]!!1 gives you 2


------------------------------------------------------------------------------
-- Ex 5: Given a list of Either String Int, sum all the integers.
-- Return Nothing if no integers were present in the list.
--
-- Examples:
--   sumEithers [Left "fail", Left "xxx"] ==> Nothing
--   sumEithers [Left "fail", Right 1, Left "xxx", Right 2] ==> Just 3

sumEithers :: [Either String Int] -> Maybe Int
sumEithers = undefined
--sumEithers [] = 0
--sumEithers x:xs = Just (x sumEithers (xs-1))
--sumEithers[xs] = Left x
--sumEithers[x] = Left x
--sumEithers(x:xs) = Right xs
--fun :: Int -> Either Int String
--sumEithers [xs] | [xs] > 0 = sum(Left [xs])
--                | otherwise = Right "-"
--sumEithers(x:xs) = Just Right xs 
----------------------
myDiv3 :: Float -> Float -> Either String Float
myDiv3 x 0 = Left "Divison by zero"
myDiv3 x y = Right (x / y)

example3 x y =
  case myDiv3 x y of
    Left msg -> putStrLn msg
    Right q  -> putStrLn (show q)

-- Example:
--  classify [Left 1, Right True, Left 0, Right False]
--     ==> ([1,0],[True,False])

--classify :: [Either a b] -> Maybe Int
--classify es = go es 
--  where go (Left a :  es) = print(5)
--        go (Right b : es) = print(6)
        
--classify :: [Either a b] -> ([a],[b])
-- #ifdef sol
classify :: [Either a b] -> ([a],[b])
classify es = go es [] []
  where go (Left a : es) as bs = go es (a:as) bs
        go (Right b : es) as bs = go es as (b:bs)
        --go [] as bs = (reverse as, reverse bs)
        go [] as bs = (reverse as, reverse bs)
       --go [] as bs = (Nothing, sum(Just bs))

----------------------

pairOff :: Int -> Either String Int
pairOff people
              | people < 0  = Left "Can't pair off negative number of people."
              | people > 30 = Left "Too many people for this activity."
              | even people = Right (people `div` 2)
              | otherwise   = Left "Can't pair off an odd number of people."

divideBy x y = go x y 0
  where go a b count
                    | a < b = (a + b)
                    | otherwise = go (a - b) b (count + 1)
------
-- selectSum [2,7,5,3,9] [0,2,4]
--    Just 16
------------------------------------------
   --sum' :: (Num a) => [a] -> a  
   --sum' [] = 0  
   --sum' (x:xs) = x + sum' xs 
------------------------------------------

--selectSum :: [Int] -> Maybe int
--selectSum is = sum(Just is)

--selectSum :: Num a => [a] -> [Int] -> Maybe a
--selectSum xs is = liftM sum $ mapM (safeIndex xs) is

--selectSum :: [int] -> Maybe int
--selectSum xs = sum $ (safeIndex xs (length xs)) 

safeIndex :: [int] -> Int -> Maybe int
safeIndex [] _ = Nothing
safeIndex (x:xs) 0 = Just x
safeIndex (x:xs) n = safeIndex xs (n-1)

iterateE :: (a -> Either b a) -> a -> b
iterateE f x = case f x of Left y  -> y
                           Right y -> iterateE f y

--step :: Int -> Int -> Either Int Int
--step k x = if x>k then Left x else Right (2*x)
--step k x = if x>k then Left x else Right x

sum2d :: [[Int]] -> Int
sum2d []           = 0
sum2d ([]:xss)     = sum2d xss
sum2d ((x:xs):xss) = x + sum2d (xs:xss)

sum1d :: [Int] -> Int
sum1d []           = 0
--sum1d ([])     = sum1d xs
sum1d (x:xs) = x + sum1d (xs)


goThru :: [a] -> Either a [a]
goThru [x] = Left x
goThru (x:xs) = Right xs --fmap (++) xs 

fun :: Int -> Either Int String
fun x | x > 0 = Left x
      | otherwise = Right "-"

--sumEithers xs | xs > 0 = Right x
 --             |otherwise = Right Nothing



------------------------------------------------------------------------------
-- Ex 6: Define the data structure Shape with values that can be
-- either circles or rectangles. A circle has just a radius, and a
-- rectangle has a width and a height.
--
-- Use _two_ constructors, one for circles, one for rectangles.
--
-- Implement the function area that computes the area of a Shape
--
-- Also implement the functions circle and rectangle that create
-- circles and rectangles (don't worry if this feels stupid, I need
-- these for the tests :)
--
-- All dimensions should be Doubles.

--data Shape = Undefined
--  deriving Show -- leave this line in place

data Shape = Circle Double | Rectangle Double Double
  deriving Show -- leave this line in place

--circle :: Double -> Shape
--circle = undefined

circle :: Double -> Shape
circle r = Circle r


--rectangle :: Double -> Double -> Shape
--rectangle = undefined

rectangle :: Double -> Double -> Shape
rectangle x1 y1 = Rectangle x1 y1

area :: Shape -> Double
--area = undefined

area (Circle r) = pi * r ^ 2
area (Rectangle x1 y1) = x1 * y1  
--area (Rectangle x1 y1) = (abs $ x1) * (abs $ y1)

-------------------------------------
--data Shape = Circle Float Float Float | Rectangle Float Float Float Float

--Circle :: Float -> Float -> Float -> Shape

--Rectangle :: Float -> Float -> Float -> Float -> Shape

--surface :: Shape -> Float  
--surface (Circle _ _ r) = pi * r ^ 2  
--surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1) 

--ghci> surface $ Circle 10 20 10  
--314.15927  
--ghci> surface $ Rectangle 0 0 100 100  
--10000.0

--data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show) 










------------------------------------------------------------------------------
-- Ex 7: Here's a Card type for a deck of cards with just two suits
-- and a joker. Implement Eq and Ord instances for Card.
--
-- The Ord instance should order cards such that
--   - Cards of the same suit are ordered according to value
--   - Suits are ordered Heart > Spade
--   - Joker is the largest card
--
-- Examples:
--   Spade 1 == Spade 2 ==> False
--   sort [Heart 3, Spade 2, Joker, Heart 1] ==> [Spade 2,Heart 1,Heart 3,Joker]

data Card = Heart Int | Spade Int | Joker
  deriving Show

instance Eq Card where
  Heart s1 == Heart s2 = s1 == s2
  Spade spa1 == Spade spa2 = spa1 == spa2
  Joker == Joker = True
  _ == _ = False
 -- Joker == Joker = True
 -- a == b = undefined -- implement me!
--                       

instance Ord Card where
  -- implement me!
   --Heart hs < Spade ss = hs > ss --False
   Spade ss <= Heart hs = True
   --Heart hs <= Heart hs2 = hs <= hs2 --True
   --Spade ss <= Spade ss2 = ss <= ss2 -- True
   --Heart > Spade = True
   Heart hs <= Spade ss = False
   Heart hs <= Joker = True
   Spade ss <= Joker = True
   Heart hs <= Heart hs2 = hs <= hs2
   Spade ss <= Spade ss2 = ss <= ss2
   x <= y        = x == y

   --Heart hs < Spade ss = False
   --Heart hs <= Heart hs2 = True
   --Spade ss <= Spade ss2 = True
   --Heart > Spade = True
   --Joker > Heart hs = True
   --compare Heart Heart = EQ
   ------------
   ------------
data Foo = Bar | Quux | Xyzzy
  deriving Show

instance Eq Foo where
  Bar == Bar     = True
  Quux == Quux   = True
  Xyzzy == Xyzzy = True
  _ == _         = False

------------------------------------------------------------------------------
-- Ex 7: implement an Ord instance for Foo so that Quux < Bar < Xyzzy

instance Ord Foo where
  Quux <= Bar   = True
  Bar <= Xyzzy  = True
  Quux <= Xyzzy = True
  x <= y        = x == y


------------------------------------------------------------------------------
-- Ex 8: Here's a type Twos for things that always come in pairs. It's
-- like a list, but it has an even number of elements (and is also
-- never empty).
--
-- Implement a Functor instance for Twos.

data Twos a = End a a | Continue a a (Twos a)
  deriving (Show, Eq)

instance Functor Twos where
  fmap f (End a b) = End (f a)(f b) -- $ f a
  fmap f (Continue a b twos) = Continue (f a) (f b) (fmap f twos)


  --data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving (Show)

    --instance Functor Tree where
    
    --fmap f (Leaf x)            = Leaf   (f x)
    --fmap f (Branch left right) = Branch (fmap f left) (fmap f right)

  --fmap _ (Continue a b c) = (Continue a b c)
  --fmap f (End a a) = (End a a)
  --fmap f (End d e) = End d (f e)
   --fmap (Continue c d e )(End a b)
   --fmap _ (End a a) = End a a
   --fmap _ (Continue a a a) = Continue a a a
   --fmap f (Bloor b) = Bloor (f b)

 -- fmap succ input === output
 -- input = Continue a b (End c d)
 -- output = Continue (succ a) (succ b) (End (succ c) (succ d))
  -- implement me!
  --(==) (Twos x x') (Twos y y') = x == y && x' == y'

--    counterexample ("fmap succ "++show input) $ fmap succ input === output
--    where input = Continue (a::Int) b (End c d)
--          output = Continue (succ a) (succ b) (End (succ c) (succ d))


------------------------------------------------------------------------------
-- Ex 9: Use the state monad to update the state with the sum of the
-- even numbers in a list. Do this by implementing the step function
-- below so that the sumEvens operation works correctly.
--
-- Examples:
--   execState (sumEvens [1,2,3,4]) 0
--   6

step :: Int -> State Int ()
--step = undefined
--step 0 = put(0)
--step (odd x) = put(0)
--step (even x) = do x2 <- get
--                   put (x+x2)
            --y <- get
            --put (y+1)
step i = do old <- get
            if even i then
               put (old+i)
            else put (old)


  --put (x)
--step x = if x ´mod´ 2 == 0 then put (x)
--         else put(0)         

sumEvens :: [Int] -> State Int ()
sumEvens is = forM_ is step


------------------------------------------------------------------------------
-- Ex 10: Here's a type Env for values that depend on an environment
-- (represented here by just a String). You'll also find some
-- utilities and example operations of type Env.
--
-- Your job is to define Functor and Monad instances for Env.
--
-- Examples of how the instances should work:
--
--
--   runEnv (fmap (+1) (return 3)) "env" ==> 4
--   runEnv (fmap (*2) envLength) "boing" ==> 10
--   runEnv (return 3) "env" ==> 3
--   runEnv (envLength >>= multiply) "xyz" ==> "xyzxyzxyz"
--   runEnv (greet >>= \g -> return ("The greeting is: "++g)) "bob"
--     ==> "The greeting is: Hello, bob"
--
-- Hint: consider W5 ex16

data Env a = MkEnv (String -> a)

runEnv :: Env a -> String -> a
runEnv (MkEnv f) str = f str

-- return a greeting for the name in the environment
greet :: Env String
greet = MkEnv (\name -> "Hello, "++name)

-- return the length of the environment
envLength :: Env Int
envLength = MkEnv (\name -> length name)

-- return a string consisting of n copies of the env
multiply :: Int -> Env String
multiply n = MkEnv (\name -> concat (replicate n name))

instance Functor Env where

   --fmap = undefined -- implement me

   fmap f (MkEnv g) = MkEnv (f.g)

instance Monad Env where
  e >>= f = join (fmap f e)
  return x = undefined 
  --return x = pure

  --instance Monad (Either a) where
  --(Right x) >>= k = k x
  --(Left err) >>= k = Left err
  --return = Right

  --return x = x
--  e >>= f = MkEnv (f e)
--  return x = MkEnv x

--instance Monad Env where
--  e >>= f = multiply (to e >>= to . f) -- (replicate (e) f)
--  return x = multiply (return x) --const --(\name -> (x, name))

   --  runEnv (envLength >>= multiply) s === concat (replicate (length s) s)
   -- e >>= f = undefined
   -- return x = undefined

   -- Disregard this instance. In recent versions of the Haskell standard
   -- library, all Monads must also be Applicative. These exercises don't
   -- really cover Applicative.
instance Applicative Env where
  pure = return
  (<*>) = ap