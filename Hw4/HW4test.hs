-- HW3Test.hs - Test suite for HW3.hs
-- To run execute:
-- 1. ghci HW3Test.hs
-- 2. main

-- Assuming serialization and deserialization functions are defined here

import EqMap
import EqSet
import GHC.Base (VecElem (Int16ElemRep))
import HW4

-- General check function that returns a tuple with a Bool for success and a message for failure
check :: (Eq a, Show a) => String -> a -> a -> (Bool, String)
check testName output expected =
  if output == expected
    then (True, "")
    else (False, testName ++ " Failed: Expected " ++ show expected ++ ", but got " ++ show output)

roundTrip :: (Eq a, Show a, Serializable a) => a -> Bool
roundTrip x = deserialize (serialize x) == x

-- List of all test cases, structured for easy extension
testCases :: [IO (Bool, String)]
testCases =
  [ -- Test EqSet
    return $ check "EqSet Insert" (EqSet.elems $ EqSet.insert 1 (EqSet.insert 2 EqSet.empty)) [1, 2],
    return $ check "EqSet Remove" (EqSet.elems $ EqSet.remove 1 (EqSet.insert 1 (EqSet.insert 2 EqSet.empty))) [2],
    return $ check "EqSet Member" (EqSet.member 1 (EqSet.insert 1 (EqSet.insert 2 EqSet.empty))) True,
    return $ check "EqSet Member False" (EqSet.member 3 (EqSet.insert 1 (EqSet.insert 2 EqSet.empty))) False,
    -- Test EqMap
    let eqMap = foldr (uncurry EqMap.insert) EqMap.empty ([(1, 'a'), (2, 'b')] :: [(Int, Char)])
     in return $ check "EqMap Insert and Lookup" (EqMap.lookup 1 eqMap) (Just 'a'),
    let eqMap = foldr (uncurry EqMap.insert) EqMap.empty ([(1, 'a'), (2, 'b')] :: [(Int, Char)])
     in return $ check "EqMap Remove" (EqMap.lookup 1 (EqMap.remove 1 eqMap)) Nothing,
    let eqMap = foldr (uncurry EqMap.insert) EqMap.empty ([(1, 'a'), (2, 'b')] :: [(Int, Char)])
     in return $ check "EqMap Member" (EqMap.member 1 eqMap) True,
    let eqMap = foldr (uncurry EqMap.insert) EqMap.empty ([(1, 'a'), (2, 'b')] :: [(Int, Char)])
     in return $ check "EqMap Member False" (EqMap.member 3 eqMap) False,
    let eqMap = foldr (uncurry EqMap.insert) EqMap.empty ([(1, 'a'), (2, 'b')] :: [(Int, Char)])
     in return $ check "EqMap Assocs" (EqMap.assocs eqMap) [(1, 'a'), (2, 'b')],
    -- Test serialization and deserialization
    return $ check "Serialize/Deserialize Int" (roundTrip (42 :: Int)) True,
    return $ check "Serialize/Deserialize Bool True" (roundTrip True) True,
    return $ check "Serialize/Deserialize Bool False" (roundTrip False) True,
    return $ check "Serialize/Deserialize Char" (roundTrip 'A') True,
    return $ check "Serialize/Deserialize Maybe Nothing" (roundTrip (Nothing :: Maybe Int)) True,
    return $ check "Serialize/Deserialize Maybe Just" (roundTrip (Just 42 :: Maybe Int)) True,
    return $ check "Serialize/Deserialize Maybe Just True" (roundTrip (Just True :: Maybe Bool)) True,
    return $ check "Serialize/Deserialize Tuple" (roundTrip ((42, True) :: (Int, Bool))) True,
    return $ check "Serialize/Deserialize Either Left" (roundTrip (Left 42 :: Either Int Bool)) True,
    return $ check "Serialize/Deserialize Either Right" (roundTrip (Right True :: Either Int Bool)) True,
    return $ check "Serialize/Deserialize List" (roundTrip ([1, 2, 3] :: [Int])) True,
    -- Test distance functions
    return $ check "Distance Int" (distance (1 :: Int) (2 :: Int)) 1.0,
    return $ check "Distance Double" (distance (1.0 :: Double) (2.0 :: Double)) 1.0,
    return $ check "Distance Char" (distance ('a' :: Char) ('b' :: Char)) 1.0,
    return $ check "Distance Tuple" (distance ((1, 2) :: (Int, Int)) ((4, 6) :: (Int, Int))) 5.0,
    return $ check "Distance ManhattanTuple" (distance (ManhattanTuple 1 2 :: ManhattanTuple Int Int) (ManhattanTuple 4 6)) 7.0,
    -- Test sort
    return $ check "Metric Bubble Sort" (metricBubbleSort 1.5 ([4, 2, 3, 1] :: [Int])) [2, 1, 4, 3],
    return $ check "Metric Bubble Sort On" (metricBubbleSortOn id 1.5 ([4, 2, 3, 1] :: [Int])) [2, 1, 4, 3],
    return $ check "Metric Bubble Sort On" (metricBubbleSortOn (* 2) 1.5 ([4, 2, 3, 1] :: [Int])) [1, 2, 3, 4]
  ]

-- Main function to run all tests and print results selectively
main :: IO ()
main = do
  putStrLn "Running tests for HW3.hs..."
  results <- sequence testCases
  let failures = filter (not . fst) results
  if null failures
    then putStrLn "All tests passed. Congratulations!"
    else mapM_ (putStrLn . snd) failures
  putStrLn "Testing complete."
