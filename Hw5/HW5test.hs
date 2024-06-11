-- HW3Test.hs - Test suite for HW3.hs
-- To run execute:
-- 1. ghci HW3Test.hs
-- 2. main

-- Assuming serialization and deserialization functions are defined here

import Deque
import HW5
import Data.List.NonEmpty(NonEmpty(..))
-- General check function that returns a tuple with a Bool for success and a message for failure
check :: (Eq a, Show a) => String -> a -> a -> (Bool, String)
check testName output expected =
  if output == expected
    then (True, "")
    else (False, testName ++ " Failed: Expected " ++ show expected ++ ", but got " ++ show output)

q1 = Deque.pushl 1 $ Deque.pushl 2 $ Deque.pushr 3 Deque.empty
q2 = Deque.pushr 30 $ Deque.pushr 20 $ Deque.pushl 10 Deque.empty

-- List of all test cases, structured for easy extension
testCases :: [IO (Bool, String)]
testCases =
  [ 
    return $ check "sum test" (foldMap' fmsum [1, 2, 3]) 6,
    return $ check "or test 1" (foldMap' fmor [False, True, False]) True,
    return $ check "or test 2" (foldMap' fmor [False, False, False]) False,
    return $ check "fold test" (foldMap' fmfold $ map show [1, 2, 3]) "123",
    return $ check "elem test" (foldMap' (fmelem 2) [1, 2, 3]) True,
    return $ check "find test 1" (foldMap' (fmfind (> 2)) [1, 2, 3]) (Just 3),
    return $ check "find test 2" (foldMap' (fmfind (> 3)) [1, 2, 3]) Nothing,
    return $ check "length test" (foldMap' fmlength [1, 2, 3]) 3,
    return $ check "null test 1" (foldMap' fmnull [1, 2, 3]) False,
    return $ check "null test 2" (foldMap' fmnull []) True,
    return $ check "toList test 1" (foldMap' fmtoList [1, 2, 3, 4, 5]) [1, 2, 3, 4, 5],
    return $ check "toList test 2" (foldMap' fmtoList "Hello") "Hello",
    return $ check "toList test 3" (foldMap' fmtoList $ 1 :| [2, 3]) [1, 2, 3]

    
    
  ]

-- Main function to run all tests and print results selectively
main :: IO ()
main = do
  putStrLn "Running tests for HW5.hs..."
  results <- sequence testCases
  let failures = filter (not . fst) results
  if null failures
    then putStrLn "All tests passed. Congratulations!"
    else mapM_ (putStrLn . snd) failures
  putStrLn "Testing complete."
