--
--
--
import Control.Concurrent

import qualified Gpio as Gpio


gpioPort :: String
gpioPort = "12"


main :: IO ()
main = do
  putStrLn $ "Using GPIO port " ++ gpioPort
  Gpio.portSetup gpioPort
  loop
    where
      loop = do
        Gpio.portTurnOn gpioPort
        threadDelay 500000
        Gpio.portTurnOff gpioPort
        threadDelay 500000
        loop
