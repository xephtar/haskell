module Main where

import System.Environment
import Data.Char

chr2dec :: String -> Int
chr2dec val
  | val' < 58 = val' - 48
  | otherwise = val' - 55
  where
    val' = ord (head val)

digs :: Int -> Int -> [Int]
digs base val
  | val == 0 = []
  | otherwise = digs base (val `div` base) ++ [val `mod` base]

data Action = D2C | C2D | L2N | N2L | ADD deriving (Eq, Show)

convertAction :: String -> Action
convertAction a = case a of
    "d2c" -> D2C
    "c2d" -> C2D
    "l2n" -> L2N
    "n2l" -> N2L
    "add" -> ADD

s2i :: String -> Int
s2i a = read a :: Int

d2cCaller :: Int -> Int -> IO()
d2cCaller base val
  | base <= val = putStrLn $ "invalid digit"
  | otherwise = putStrLn $ show (d2c val)

c2dCaller :: Int -> String -> IO()
c2dCaller base val
  | base <= (chr2dec val) = putStrLn $ "invalid digit"
  | otherwise = putStrLn $ show (chr2dec val)

takePow :: Int -> Int -> Int -> [String] -> Int
takePow base len result xs
  | base == 0 = 0
  | len > 0 = takePow base (length (tail xs)) (result + ((base^(len-1)) * (s2i (head xs)))) (tail xs)
  | otherwise = result

l2n :: Int -> Int -> [String] -> IO()
l2n base len val = putStrLn $ show (takePow base len 0 val)

d2c :: Int -> Char
d2c val
  | val < 10 = chr (val + 48)
  | otherwise = chr (val + 55)

digitToChar :: [Int] -> String
digitToChar [] = ""
digitToChar (x:xs) = [(d2c x)] ++ (digitToChar xs)

printDigitsAndCharacters :: [String] -> Int -> IO ()
printDigitsAndCharacters [] base = return ()
printDigitsAndCharacters (x:xs) base =
 do
    let val = s2i x
    let digits = digs base val
    putStrLn $ show (digits)
    putStrLn $ show (digitToChar digits)
    printDigitsAndCharacters xs base

addNumbers :: [String] -> Int
addNumbers [] = 0
addNumbers (x:xs) = (s2i x) + addNumbers xs

printResult :: [String] -> Int -> IO ()
printResult [] _ = 
  do
    putStrLn $ show ("[]")
    putStrLn $ show (0)
printResult numbers base =
  do
    let additionOfNumbers = addNumbers numbers
    let digits = digs base additionOfNumbers
    putStrLn $ show (digits)
    putStrLn $ show (additionOfNumbers)

main = do
  args <- getArgs
  if length args == 0
    then do putStrLn "Argument is not given!"
    else do
      if length args >= 3 then do
        let action = convertAction (args !! 0)
        let base = s2i (args !! 1)
        case action of
            D2C -> do
              let val = s2i (args !! 2)
              d2cCaller base val            
            C2D -> do 
              let val = (args !! 2)
              c2dCaller base val
            L2N -> do
              let given = tail (tail args) 
              l2n base (length given) given
            N2L -> do 
              let val = s2i (args !! 2)
              putStrLn $ show (digs base val)
            ADD -> do
              let numbers = tail (tail args)
              printDigitsAndCharacters numbers base
              printResult numbers base

      else do putStrLn "Arguments should be like that \"d2c base decimal_value\"!"