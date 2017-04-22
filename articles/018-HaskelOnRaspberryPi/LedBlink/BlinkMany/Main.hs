--
--
--
import Control.Concurrent ( threadDelay )
import Control.Exception ( try )
import Control.Monad ( forever )
import qualified Gpio as Gpio


data Pattern = Pattern [Bool]
data PatternSeq = PatternSeq [Pattern]

bitPatterns = [
    "10000000",
    "01000000",
    "00100000",
    "00010000",
    "00001000",
    "00000100",
    "00001000",
    "00010000",
    "00100000",
    "01000000",
    "10000000",

    "11000000",
    "11100000",
    "11110000",
    "11111000",
    "11111100",
    "11111000",
    "11110000",
    "11100000",
    "11000000"
    ]

portNumbers = ["7", "1", "12", "16", "20", "21"]

delay = 100000 -- micro seconds


main :: IO ()
main = do
  putStrLn "Starting..."
  ports <- sequence $ map Gpio.outPort portNumbers
  let patterns = PatternSeq $ map bitsToPattern bitPatterns
  cycleForever ports patterns


cycleForever :: [Gpio.Port] -> PatternSeq -> IO ()
cycleForever ports (PatternSeq patterns) =
  mapM_ (doOnePattern ports) $ cycle patterns


doOnePattern :: [Gpio.Port] -> Pattern -> IO ()
doOnePattern ports pattern = do
  setPattern ports pattern
  threadDelay delay


setPattern :: [Gpio.Port] -> Pattern -> IO ()
setPattern ports (Pattern values) = do
  sequence $ zipWith Gpio.set values ports
  return ()


bitsToPattern :: [Char] -> Pattern
bitsToPattern  bits =
  Pattern $ map (\c -> c/='0') bits
