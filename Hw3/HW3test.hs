-- HW3Test.hs - Test suite for HW3.hs
-- To run execute:
-- 1. ghci HW3Test.hs
-- 2. main

import HW3 -- Assuming serialization and deserialization functions are defined here

-- General check function that returns a tuple with a Bool for success and a message for failure
check :: (Eq a, Show a) => String -> a -> a -> (Bool, String)
check testName output expected =
  if output == expected
    then (True, "")
    else (False, testName ++ " Failed: Expected " ++ show expected ++ ", but got " ++ show output)

-- List of all test cases, structured for easy extension
testCases :: [IO (Bool, String)]
testCases =
  [ -- Test serialization and deserialization
    return $
      check
        "Serialize/Deserialize Test 1"
        (deserialize (serialize (Tree (Tree Empty 1 Empty) 2 (Tree Empty 3 Empty))))
        (Tree (Tree Empty 1 Empty) 2 (Tree Empty 3 Empty)),
    return $
      check
        "Serialize/Deserialize Test with negative values"
        (deserialize (serialize (Tree (Tree Empty (-1) Empty) (-2) (Tree Empty (-3) Empty))))
        (Tree (Tree Empty (-1) Empty) (-2) (Tree Empty (-3) Empty)),
    return $
      check
        "Serialize/Deserialize Test 2"
        (deserialize (serialize (Tree Empty 1 (Tree (Tree Empty 2 Empty) 3 Empty))))
        (Tree Empty 1 (Tree (Tree Empty 2 Empty) 3 Empty)),
    return $
      check
        "Serialize/Deserialize Test 3"
        (deserialize (serialize Empty))
        Empty,
    return $
      check
        "Serialize/Deserialize Test 4"
        (deserialize (serialize (Tree Empty 0 Empty)))
        (Tree Empty 0 Empty),
    -- Test sample and smallSample on InfiniteList
    return $
      check
        "InfiniteList sample Test"
        (sample $ irepeat 3)
        [3, 3, 3, 3, 3, 3, 3, 3, 3, 3],
    return $
      check
        "InfiniteList smallSample Test"
        (smallSample $ irepeat 3)
        [3, 3, 3, 3, 3],
    return $
      check
        "InfiniteList smallSample irepeat Test"
        (smallSample $ irepeat 1)
        [1, 1, 1, 1, 1],
    return $
      check
        "InfiniteList smallSample iiterate Test"
        (smallSample $ iiterate (\x -> x * x + x) 1)
        [1, 2, 6, 42, 1806],
    return $
      check
        "InfiniteList sample naturals Test"
        (sample naturals)
        [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],
    return $
      check
        "InfiniteList itake naturals Test"
        (itake 5 naturals)
        [0, 1, 2, 3, 4],
    return $
      check
        "InfiniteList sample idrop naturals Test"
        (sample $ idrop 5 naturals)
        [5, 6, 7, 8, 9, 10, 11, 12, 13, 14],
    return $
      check
        "InfiniteList sample imap naturals Test"
        (sample $ imap (* 3) naturals)
        [0, 3, 6, 9, 12, 15, 18, 21, 24, 27],
    return $
      check
        "InfiniteList sample iprepend naturals Test"
        (sample $ iprepend [1, 2, 3] naturals)
        [1, 2, 3, 0, 1, 2, 3, 4, 5, 6],
    return $
      check
        "InfiniteList sample ifilter even naturals Test"
        (sample $ ifilter even naturals)
        [0, 2, 4, 6, 8, 10, 12, 14, 16, 18],
    return $
      check
        "InfiniteList smallSample ifilter naturals Test"
        (smallSample $ ifilter (< 5) naturals)
        [0, 1, 2, 3, 4],
    return $
      check
        "InfiniteList sample iconcat iiterate Test"
        (sample $ iconcat $ iiterate (map (+ 1)) [1, 2, 3])
        [1, 2, 3, 2, 3, 4, 3, 4, 5, 4],
    return $
      check
        "InfiniteList smallSample iconcat Test"
        (smallSample $ iconcat $ [1] :> [2] :> [3] :> [4] :> [5] :> irepeat [])
        [1, 2, 3, 4, 5],
    return $
      check
        "InfiniteList ifind Test"
        (ifind (> 10) naturals)
        11,
    return $
      check
        "parseAndRun Test 1"
        (parseAndRun "PUSH 1\nPUSH 2\nBad!\nNo!")
        (Left (ParseError "Bad!")),
    return $
      check
        "parseAndRun Test 2"
        (parseAndRun "PUSH 2\nPOP\nPOP")
        (Left (InstructionError (StackUnderflow {instruction = "POP", stackValue = Nothing}))),
    return $
      check
        "parseAndRun Test 3"
        (parseAndRun "PUSH 0\nPUSH 1\nDIV")
        (Left (InstructionError DivisionByZero)),
    return $
      check
        "parseAndRun Test 4"
        (parseAndRun "PUSH 2\nPUSH 1\nSWAP")
        (Right [2, 1]),
    return $
      check
        "parseAndRun Test 5"
        (parseAndRun "PUSH 2\nPUSH 4\nMUL\nPUSH 5\nSUB\nPUSH 3")
        (Right [3, -3])
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
