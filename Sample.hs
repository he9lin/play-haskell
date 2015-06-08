import System.Environment
import Text.Printf

mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element: " ++ show x
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You're underweight, you emo, you!"
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
    | otherwise   = "You're a whale, congratulations!"

bmiTell2 :: (RealFloat a) => a -> a -> String
bmiTell2 weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
          skinny = 18.5
          normal = 25.0
          fat = 30.0

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

main = do
  putStrLn "Hello, World"
  -- [d] <- map read `fmap` getArgs
  -- printf "%f\n" (mean [1 .. d])
  let arrayInts = [4, 5, 6]

  print $ head' arrayInts
  print $ length' arrayInts
  print $ tell [1, 2]
  print $ bmiTell 12.5
  print $ bmiTell2 300.5 1
  print $ map (Circle 10 20) [4,5,6,6]
