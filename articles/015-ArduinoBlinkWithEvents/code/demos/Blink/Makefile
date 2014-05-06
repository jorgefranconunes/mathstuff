###########################################################################
#
# Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
#
###########################################################################

include ../../make/defs.make


B_PROJ = Blink

B_CC       = avr-gcc
B_CC_FLAGS = -g -Os -Wall -std=gnu99 -mmcu=atmega328p

B_LD     = avr-gcc
B_AR     = avr-ar
B_RANLIB = avr-ranlib


#
# The main sources, to be compiled with AVR-GCC.
#

B_MAIN_CC_FLAGS = \
	$(B_CC_FLAGS) \
	-I ../../ardev-core/src/main/c \
	-I ../../ardev-atm328p/src/main/c

B_MAIN_LD_FLAGS = \
	-L ../../ardev-atm328p/target \
	-lardev-atm328p-$(B_PROJ_VERSION) \
	-L ../../ardev-core/target \
	-lardev-core-$(B_PROJ_VERSION)

B_MAIN_C_SOURCES = \
	Blink.c


include ../../make/app.make 


all : ../../bin/$(B_PROJ).hex

../../bin/$(B_PROJ).hex : $(B_MAIN_TARGET)
	avr-objcopy -O ihex -R .eeprom $< $@

