import System.Environment


main :: IO ()
main = do
  args <- getArgs
  putStrLn $ formatArgCount args
  putStr $ formatAllArgs args


formatArgCount :: [String] -> String
formatArgCount xs = "Argument count: " ++ argCount
  where argCount = show $ length xs


formatAllArgs :: [String] -> String
formatAllArgs = unlines . map formatOneArg
  where formatOneArg = (++) "    "
