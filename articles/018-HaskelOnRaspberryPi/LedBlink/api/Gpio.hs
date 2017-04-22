--
--
--
module Gpio (
  Port,
  outPort,
  set,
  turnOn,
  turnOff
  ) where

import System.Directory
import System.IO


data Port = Port
            { name :: String
            , baseDir :: String
            }


--
-- Factory function.
--
outPort :: String -> IO Port
outPort portName = do
  let port = Port {
        name = portName,
        baseDir = gpioBaseDir ++ "/gpio" ++ portName
        }
  setupAsOutput port
  return port


--
-- Prepares the GPIO port to be used as output.
--
setupAsOutput :: Port -> IO ()
setupAsOutput port = do
  portCreate port
  portSetDirection "out" port


portCreate :: Port -> IO ()
portCreate port = do
  isPortAlreadySetup <- doesDirectoryExist (baseDir port)
  if isPortAlreadySetup
     then return ()
     else portReallyCreate port


portReallyCreate :: Port -> IO ()
portReallyCreate port = do
  let exportPath = gpioBaseDir ++ "/export"
  writeFile exportPath (name port)


portSetDirection :: String -> Port -> IO ()
portSetDirection direction port = do
  let directionPath = (baseDir port) ++ "/direction"
  writeFile directionPath direction


turnOn :: Port -> IO ()
turnOn = set True


turnOff :: Port -> IO ()
turnOff = set False


set :: Bool -> Port -> IO ()
set isOn port = do
  let portPath = (baseDir port) ++ "/value"
      value = if isOn then "1" else "0"
  writeFile portPath value


gpioBaseDir :: String
gpioBaseDir = "/sys/class/gpio"
