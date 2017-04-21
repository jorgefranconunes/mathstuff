--
--
--
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Gpio as Gpio


data Pattern = Pattern [Bool]

pattern :: [Bool] -> Pattern
pattern = Pattern


data PatternSeq = PatternSeq [Pattern]

patternSeq :: [Pattern] -> PatternSeq
patternSeq = PatternSeq


patterns = patternSeq [
  pattern [True,  False, False, False, False, False],
  pattern [False, True,  False, False, False, False],
  pattern [False, False, True,  False, False, False],
  pattern [False, False, False, True,  False, False],
  pattern [False, False, False, False, True,  False],
  pattern [False, False, False, False, False, True],
  pattern [False, False, False, False, True,  False],
  pattern [False, False, False, True,  False, False],
  pattern [False, False, True,  False, False, False],
  pattern [False, True,  False, False, False, False],
  pattern [True,  False, False, False, False, False],

  pattern [True,  True,  False, False, False, False],
  pattern [True,  True,  True,  False, False, False],
  pattern [True,  True,  True,  True,  False, False],
  pattern [True,  True,  True,  True,  True,  False],
  pattern [True,  True,  True,  True,  True,  True],
  pattern [True,  True,  True,  True,  True,  False],
  pattern [True,  True,  True,  False, False, False],
  pattern [True,  True,  False, False, False, False]
  ]


portNumbers = ["7", "1", "12", "16", "20", "21"]


delay = 100000 -- micro seconds


main :: IO ()
main = do
  putStrLn "Starting..."
  ports <- sequence $ map Gpio.outPort portNumbers
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
