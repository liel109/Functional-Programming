-- HW1Test.hs - Test suite for HW1.hs
-- To run execute:
-- 1. ghci HW1test.hs
-- 2. main

import Distribution.InstalledPackageInfo qualified as HW1test
import HW1

-- General check function that returns a tuple with a Bool for success and a message for failure
check :: (Eq a, Show a) => String -> a -> a -> (Bool, String)
check testName output expected =
  if output == expected
    then (True, "")
    else (False, testName ++ " Failed: Expected " ++ show expected ++ ", but got " ++ show output)

-- List of all test cases, structured for easy extension
testCases :: [IO (Bool, String)]
testCases =
  [ return $ check "countDigits 0" (countDigits 0) 1,
    return $ check "countDigits 1024" (countDigits 1024) 4,
    return $ check "countDigits -42" (countDigits (-42)) 2,
    return $ check "toBinary 0" (toBinary 0) 0,
    return $ check "toBinary 1" (toBinary 1) 1,
    return $ check "toBinary 42" (toBinary 42) 101010,
    return $ check "toBinary -10" (toBinary (-10)) (-1010),
    return $ check "fromBinary 0" (fromBinary 0) 0,
    return $ check "fromBinary 1" (fromBinary 1) 1,
    return $ check "fromBinary 101010" (fromBinary 101010) 42,
    return $ check "fromBinary -1010" (fromBinary (-1010)) (-10),
    return $ check "fromBinary . toBinary $ 42" (fromBinary (toBinary 42)) 42,
    return $ check "toBinary . fromBinary $ 101010" (toBinary (fromBinary 101010)) 101010,
    return $ check "isAbundant 12" (isAbundant 12) True,
    return $ check "isAbundant 15" (isAbundant 15) False,
    return $ check "isAbundant 18" (isAbundant 18) True,
    return $ check "isAbundant -12" (isAbundant (-12)) False,
    return $ check "rotateDigits 1" (rotateDigits 1) 1,
    return $ check "rotateDigits 1234" (rotateDigits 1234) 2341,
    return $ check "rotateDigits -1234" (rotateDigits (-1234)) (-4123),
    return $ check "rotateDigits 102" (rotateDigits 102) 21,
    return $ check "rotateDigits -102" (rotateDigits (-102)) (-210),
    return $ check "nullGen ((+ 1), (< 0), 0)" (nullGen ((+ 1), (< 0), 0)) True,
    return $ check "nullGen ((+ 1), (<= 0), 0)" (nullGen ((+ 1), (<= 0), 0)) False,
    return $ check "nullGen ((+ 1), (<= 1), 0)" (nullGen ((+ 1), (<= 1), 0)) False,
    return $ check "nullGen positives" (nullGen positives) False,
    return $ check "lastGen ((* 2), (< 1), 1)" (lastGen ((* 2), (< 1), 1)) 1,
    return $ check "lastGen ((* 2), (<= 1), 1)" (lastGen ((* 2), (<= 1), 1)) 2,
    return $ check "lastGen ((* 2), (< 1000), 1)" (lastGen ((* 2), (< 1000), 1)) 1024,
    return $ check "lengthGen ((+ 1), (< 0), 0)" (lengthGen ((+ 1), (< 0), 0)) 0,
    return $ check "lengthGen ((+ 1), (<= 0), 0)" (lengthGen ((+ 1), (<= 0), 0)) 1,
    return $ check "lengthGen ((+ 1), (< 10), 0)" (lengthGen ((+ 1), (< 10), 0)) 10,
    return $ check "sumGen ((+ 1), (< 10), 0)" (sumGen ((+ 1), (< 10), 0)) 55,
    return $ check "sumGen ((+ 1), (< 10), 1)" (sumGen ((+ 1), (< 10), 1)) 54,
    return $ check "sumGen ((+ 1), (< 10), 10)" (sumGen ((+ 1), (< 10), 10)) 0,
    return $ check "allGen (< 10) positives" (allGen (< 10) positives) False,
    return $ check "allGen (> 0) positivesUpTo10" (allGen (> 0) positivesUpTo10) True,
    return $ check "anyGen (< 10) positives" (anyGen (< 10) positives) True,
    return $ check "anyGen (<= 0) positivesUpTo10" (anyGen (<= 0) positivesUpTo10) False,
    return $ check "noneGen (< 10) positives" (noneGen (< 10) positives) False,
    return $ check "noneGen (<= 0) positivesUpTo10" (noneGen (<= 0) positivesUpTo10) True,
    return $ check "countGen even positivesUpTo10" (countGen even positivesUpTo10) 5,
    return $ check "countGen even ((+ 1), (< 10), 1)" (countGen even ((+ 1), (< 10), 1)) 5,
    return $ check "countGen even ((+ 1), (< 9), 1)" (countGen even ((+ 1), (< 9), 1)) 4,
    return $ check "anyGen (< 11) emptyGen" (anyGen (< 11) (emptyGen)) False,
    return $ check "allGen (< 11) emptyGen" (allGen (< 11) (emptyGen)) True,
    return $ check "noneGen (< 11) emptyGen" (noneGen (< 11) (emptyGen)) True,
    -- Prime number tests
    return $ check "isPrime 1" (isPrime 1) False,
    return $ check "isPrime 2" (isPrime 2) True,
    return $ check "isPrime 29" (isPrime 29) True,
    return $ check "isPrime -2" (isPrime (-2)) False,
    -- Semiprime tests
    return $ check "isSemiprime 2" (isSemiprime 2) False,
    return $ check "isSemiprime 10" (isSemiprime 10) True,
    return $ check "isSemiprime 77" (isSemiprime 77) True,
    return $ check "isSemiprime -10" (isSemiprime (-10)) False,
    -- Goldbach pair tests
    return $ check "goldbachPair 4" (goldbachPair 4) (2, 2),
    return $ check "goldbachPair 10" (goldbachPair 10) (3, 7),
    return $ check "goldbachPair 100" (goldbachPair 100) (3, 97),
    -- Modified Goldbach pair tests
    return $ check "goldbachPair' 4" (goldbachPair' 4) (2, 2),
    return $ check "goldbachPair' 10" (goldbachPair' 10) (5, 5),
    return $ check "goldbachPair' 100" (goldbachPair' 100) (53, 47),
    -- Circular prime tests
    return $ check "isCircularPrime 5" (isCircularPrime 5) True,
    return $ check "isCircularPrime 17" (isCircularPrime 17) True,
    return $ check "isCircularPrime 103" (isCircularPrime 103) False,
    return $ check "isCircularPrime 193" (isCircularPrime 193) False,
    return $ check "isCircularPrime 197" (isCircularPrime 197) True,
    return $ check "isCircularPrime 199" (isCircularPrime 199) True
  ]

-- Main function to run all tests and print results selectively
main :: IO ()
main = do
  putStrLn "Running tests for HW1.hs..."
  results <- sequence testCases
  let failures = filter (not . fst) results
  if null failures
    then putStrLn "All tests passed. Congratulations!"
    else mapM_ (putStrLn . snd) failures
  putStrLn "Testing complete."
