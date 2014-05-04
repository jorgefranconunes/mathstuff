###########################################################################
#
# Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
#
###########################################################################

include ../make/defs.make


B_PROJ = ardev-core

B_CC       = avr-gcc
B_CC_FLAGS = -g -Os -Wall -std=gnu99 -mmcu=atmega328p

B_LD     = avr-gcc
B_AR     = avr-ar
B_RANLIB = avr-ranlib


#
# The main sources, to be compiled with AVR-GCC.
#

B_MAIN_C_SOURCES = \
	ardev/events/Event.c \
	ardev/events/EventListener.c \
	ardev/events/EventManager.c \
	ardev/events/EventSource.c \
	ardev/events/EventType.c \
	ardev/tasks/CallbackTask.c \
	ardev/tasks/CounterTaskService.c \
	ardev/tasks/Task.c \
	ardev/tasks/TaskScheduler.c \
	ardev/tasks/TaskService.c \
	ardev/ticks/Clock.c \
	ardev/ticks/CounterClock.c \
	ardev/ticks/CounterTickSource.c \
	ardev/ticks/TickEventType.c


include ../make/lib.make

