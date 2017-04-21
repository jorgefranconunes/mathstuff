#!/bin/bash

portNumber=12
blinkHalfPeriod=0.5
portBaseDir=/sys/class/gpio/gpio${portNumber}

# Setup port  for output
if [ ! -d ${portBaseDir} ] ; then
    echo ${portNumber} > /sys/class/gpio/export
fi
echo out > ${portBaseDir}/direction

# Blink forever
while true ; do
    echo 1 > ${portBaseDir}/value
    sleep ${blinkHalfPeriod}
    echo 0 > ${portBaseDir}/value
    sleep ${blinkHalfPeriod}
done
