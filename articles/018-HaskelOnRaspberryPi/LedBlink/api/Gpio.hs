--
--
--
module Gpio
( portSetup
, portTurnOn
, portTurnOff
) where

import System.Directory
import System.IO


portSetup :: String -> IO ()
portSetup port = do
  portCreate port
  portSetAsOutput port


portCreate :: String -> IO ()
portCreate port = do
  let portBaseDir = gpioPortBaseDir port
  isPortAlreadySetup <- doesDirectoryExist portBaseDir
  if isPortAlreadySetup
     then return ()
     else portReallyCreate port


portReallyCreate :: String -> IO ()
portReallyCreate port = do
  let exportPath = gpioBaseDir ++ "/export"
  writeFile exportPath port


portSetAsOutput :: String -> IO ()
portSetAsOutput port = do
  let directionPath = (gpioPortBaseDir port) ++ "/direction"
  writeFile directionPath "out"


portTurnOn :: String -> IO ()
portTurnOn port = portSet port True


portTurnOff :: String -> IO ()
portTurnOff port = portSet port False


portSet :: String -> Bool -> IO ()
portSet port isOn = do
  let portPath = (gpioPortBaseDir port) ++ "/value"
      value = if isOn then "1" else "0"
  writeFile portPath value


gpioBaseDir :: String
gpioBaseDir = "/sys/class/gpio"


gpioPortBaseDir :: String -> String
gpioPortBaseDir port = gpioBaseDir ++ "/gpio" ++ port
