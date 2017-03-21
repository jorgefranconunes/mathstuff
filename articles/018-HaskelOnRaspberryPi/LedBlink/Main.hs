--
--
--
import Control.Concurrent
import System.Directory
import System.IO


gpioPort :: String
gpioPort = "12"


main :: IO ()
main = do
  putStrLn $ "Using GPIO port " ++ gpioPort
  gpioSetupPort gpioPort
  loop
    where
      loop = do
        gpioPortTurnOn gpioPort
        threadDelay 500000
        gpioPortTurnOff gpioPort
        threadDelay 500000
        loop


gpioSetupPort :: String -> IO ()
gpioSetupPort port = do
  gpioCreatePort port
  gpioSetOutputPort port


gpioCreatePort :: String -> IO ()
gpioCreatePort port = do
  let portBaseDir = gpioPortBaseDir port
  portAlreadySetup <- doesDirectoryExist portBaseDir
  if portAlreadySetup
     then return ()
     else gpioReallyCreatePort port


gpioReallyCreatePort :: String -> IO ()
gpioReallyCreatePort port = do
  writeFile exportPath port
    where exportPath = gpioBaseDir ++ "/export"


gpioSetOutputPort :: String -> IO ()
gpioSetOutputPort port =
  writeFile directionPath "out"
  where directionPath = (gpioPortBaseDir port) ++ "/direction"


gpioPortTurnOn :: String -> IO ()
gpioPortTurnOn port = gpioPortSet port True


gpioPortTurnOff :: String -> IO ()
gpioPortTurnOff port = gpioPortSet port False


gpioPortSet :: String -> Bool -> IO ()
gpioPortSet port isOn =
  writeFile portPath value
  where portPath = (gpioPortBaseDir port) ++ "/value"
        value = if isOn then "1" else "0"


gpioBaseDir :: String
gpioBaseDir = "/sys/class/gpio"


gpioPortBaseDir :: String -> String
gpioPortBaseDir port = gpioBaseDir ++ "/gpio" ++ port
