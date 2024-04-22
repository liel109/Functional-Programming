-- HW1Test.hs - Test suite for HW1.hs
-- To run execute:
-- 1. ghci HW1test.hs
-- 2. main

import Distribution.InstalledPackageInfo qualified as HW1test
import HW2

-- General check function that returns a tuple with a Bool for success and a message for failure
check :: (Eq a, Show a) => String -> a -> a -> (Bool, String)
check testName output expected =
  if output == expected
    then (True, "")
    else (False, testName ++ " Failed: Expected " ++ show expected ++ ", but got " ++ show output)

-- List of all test cases, structured for easy extension
testCases :: [IO (Bool, String)]
testCases =
  [ return $ check "" (concatMaybeMap (\x -> if x > 0 then Just $ x * 10 else Nothing) Nothing) Nothing,
    return $ check "" (concatMaybeMap (\x -> if x > 0 then Just $ x * 10 else Nothing) $ Just 10) 100,
    return $ check "" (fromMaybe 1 Nothing) 1,
    return $ check "" (fromMaybe 1 (Just 2)) 2,
    return $ check "" (HW2.maybe 1 length (Just "foo")) 3,
    return $ check "" (catMaybes [Just 1, Nothing, Just 3]) [1, 3],
    return $ check "" (mapMaybe (\x -> if x > 0 then Just $ x * 10 else Nothing) [1, -1, 10]) [10, 100],
    return $ check "" (HW2.either length (* 10) $ Left "foo") 3,
    return $ check "" (HW2.either length (* 10) $ Right 10) 100,
    return $ check "" (mapLeft (++ "bar") (Left "foo")) Left "foobar",
    return $ check "" (mapLeft (++ " bar ") (Right 10)) Right 10,
    return $ check "" (catEithers [Right 10, Right 20]) Right [10, 20],
    return $ check "" (catEithers [Right 10, Left "foo", Right 20, Left "bar"]) Left "foo",
    return $ check "" (mapEither (\x -> if x > 0 then Right $ x * 10 else Left $ x + 5) [1, 2, 3]) Right [10, 20, 30],
    return $ check "" (mapEither (\x -> if x > 0 then Right $ x * 10 else Left $ x + 5) [1, -1, 2, -2]) Left 4,
    return $ check "" (concatEitherMap (Right . (* 10)) (Right 5)) Right 50,
    return $ check "" (concatEitherMap (Right . (* 10)) (Left 5)) Left 5,
    return $ check "" (partitionEithers [Right "foo ", Left 42, Right " bar ", Left 54]) ([42, 54], [" foo ", " bar "])
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
