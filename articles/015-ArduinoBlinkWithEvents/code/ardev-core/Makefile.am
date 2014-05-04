#
#
#

AM_CFLAGS = -g -Os -Wall -std=gnu99

noinst_LIBRARIES = \
	libardev.a

libardev_a_SOURCES = \
	src/main/c/helloutils.c \
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

libardev_a_CPPFLAGS = \
	-I$(top_srcdir)/ardev/src/main/c





TESTS = \
	AllTests

check_PROGRAMS = \
	AllTests

AllTests_SOURCES = \
	src/test/c/ardev/events/TestEventListener.c \
	src/test/c/ardev/events/TestEventSource.c \
	src/test/c/ardev/tasks/TestClock.c \
	src/test/c/ardev/tasks/TestTask.c \
	src/test/cpp/AllTests.cpp \
	src/test/cpp/ardev/events/EventManagerTest.cpp \
	src/test/cpp/ardev/tasks/CallbackTaskTest.cpp \
	src/test/cpp/ardev/tasks/TaskSchedulerTest.cpp \
	src/test/cpp/ardev/tasks/TaskServiceTest.cpp \
	src/test/cpp/ardev/ticks/CounterClockTest.cpp \
	src/test/cpp/ardev/ticks/CounterTickSourceTest.cpp \
	src/test/cpp/ardev/ticks/TickEventTypeTest.cpp

AllTests_CPPFLAGS = \
	-I$(top_srcdir)/ardev/src/main/c \
	-I$(top_srcdir)/ardev/src/test/c \
	-I$(top_srcdir)/ardev/src/test/cpp \
	-I$(CPPUTEST)/include

AllTests_LDADD = \
	-L$(top_builddir)/ardev \
	-lardev \
	-L$(CPPUTEST)/lib \
	-lCppUTest

