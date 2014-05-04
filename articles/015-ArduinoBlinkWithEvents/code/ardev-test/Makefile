###########################################################################
#
# Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
#
###########################################################################

include ../make/defs.make


B_PROJ = ardev-test

B_CC_FLAGS = -g -Os -Wall -std=gnu99


#
# The core library, to be compiled with the host GCC.
#

B_MAIN_C_SRCDIR = ../ardev-core/src/main/c

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


#
# Unit tests, to be run on the host.
#

B_TEST_CXX_FLAGS = \
	-g \
	-I $(CPPUTEST)/include

B_TEST_LD_FLAGS = \
	-L $(CPPUTEST)/lib \
	-lCppUTest

B_TEST_C_SOURCES = \
	ardev/events/TestEventListener.c \
	ardev/events/TestEventSource.c \
	ardev/tasks/TestClock.c \
	ardev/tasks/TestTask.c

B_TEST_CXX_SOURCES = \
	AllTests.cpp \
	ardev/events/EventManagerTest.cpp \
	ardev/tasks/CallbackTaskTest.cpp \
	ardev/tasks/TaskSchedulerTest.cpp \
	ardev/tasks/TaskServiceTest.cpp \
	ardev/ticks/CounterClockTest.cpp \
	ardev/ticks/CounterTickSourceTest.cpp \
	ardev/ticks/TickEventTypeTest.cpp


include ../make/lib.make

