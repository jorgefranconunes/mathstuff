--
--
--
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Gpio as Gpio


portNumber = "12"
blinkHalfPeriod = 500000 -- micro seconds


main :: IO ()
main = do
  putStrLn $ "Using GPIO port " ++ portNumber
  result <- try (Gpio.outPort portNumber) :: IO (Either IOException Gpio.Port)
  case result of
    Left e ->
      putStrLn $ "Failed to initialize port " ++ portNumber ++ " - " ++ (show e)
    Right port ->
      blinkForever port


blinkForever :: Gpio.Port -> IO ()
blinkForever port = do
  forever $ do
    Gpio.turnOn port
    threadDelay blinkHalfPeriod
    Gpio.turnOff port
    threadDelay blinkHalfPeriod
