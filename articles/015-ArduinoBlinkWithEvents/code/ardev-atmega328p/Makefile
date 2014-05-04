###########################################################################
#
# Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
#
###########################################################################

include ../make/defs.make


B_PROJ = ardev-atmega328p

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
	-I ../ardev-core/src/main/c

B_MAIN_C_SOURCES = \
	ardev/tasks/atmega328p/Atmega328pTaskService.c \
	ardev/ticks/atmega328p/Atmega328pTickSource.c \
	ardev/events/atmega328p/Atmega328pEventManager.c


include ../make/lib.make

