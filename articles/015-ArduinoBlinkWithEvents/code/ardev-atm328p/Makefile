###########################################################################
#
# Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
#
###########################################################################

include ../make/defs.make


B_PROJ = ardev-atm328p

B_CC       = avr-gcc
B_CC_FLAGS = -g -Os -Wall -std=gnu99 -mmcu=atmega328p -DNDEBUG

B_LD     = avr-gcc
B_AR     = avr-ar
B_RANLIB = avr-ranlib


#
# The main sources, to be compiled with AVR-GCC.
#

B_MAIN_CC_FLAGS = \
	$(B_CC_FLAGS) \
	-I ../ardev-core/src/main/c

B_MAIN_C_SOURCES = \
	ardev/sys/atm328p/Atm328pTaskService.c \
	ardev/sys/atm328p/Atm328pTickSource.c


include ../make/lib.make

