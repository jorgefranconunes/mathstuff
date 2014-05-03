###########################################################################
#
# Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
#
###########################################################################

include ../make/defs.make


B_PROJ         = ardev
B_PROJ_VERSION = 0.0.1

B_CC_FLAGS = -g -Os -Wall -std=gnu99

B_MAIN_C_SRC = \
	src/main/c/ardev/events/Event.c \
	src/main/c/ardev/events/EventListener.c \
	src/main/c/ardev/events/EventManager.c \
	src/main/c/ardev/events/EventSource.c \
	src/main/c/ardev/events/EventType.c \
	src/main/c/ardev/tasks/CallbackTask.c \
	src/main/c/ardev/tasks/CounterTaskService.c \
	src/main/c/ardev/tasks/Task.c \
	src/main/c/ardev/tasks/TaskScheduler.c \
	src/main/c/ardev/tasks/TaskService.c \
	src/main/c/ardev/ticks/Clock.c \
	src/main/c/ardev/ticks/CounterClock.c \
	src/main/c/ardev/ticks/CounterTickSource.c \
	src/main/c/ardev/ticks/TickEventType.c

B_MAIN_CXX_SRC =



B_CXX_FLAGS_TEST = \
	-g \
	-I$(CPPUTEST)/include

B_LD_FLAGS_TEST = \
	-L$(CPPUTEST)/lib \
	-lCppUTest

B_TEST_C_SRC = \
	src/test/c/ardev/events/TestEventListener.c \
	src/test/c/ardev/events/TestEventSource.c \
	src/test/c/ardev/tasks/TestClock.c \
	src/test/c/ardev/tasks/TestTask.c

B_TEST_CXX_SRC = \
	src/test/cpp/AllTests.cpp \
	src/test/cpp/ardev/events/EventManagerTest.cpp \
	src/test/cpp/ardev/tasks/CallbackTaskTest.cpp \
	src/test/cpp/ardev/tasks/TaskSchedulerTest.cpp \
	src/test/cpp/ardev/tasks/TaskServiceTest.cpp \
	src/test/cpp/ardev/ticks/CounterClockTest.cpp \
	src/test/cpp/ardev/ticks/CounterTickSourceTest.cpp \
	src/test/cpp/ardev/ticks/TickEventTypeTest.cpp


include ../make/lib.make

