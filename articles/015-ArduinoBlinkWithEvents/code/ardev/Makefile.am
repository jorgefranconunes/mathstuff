#
#
#

AM_CFLAGS = -g -Os -Wall -std=gnu99

noinst_LIBRARIES = \
	libardev.a

libardev_a_SOURCES = \
	main/helloutils.c \
	main/ardev/events/Event.c \
	main/ardev/events/EventListener.c \
	main/ardev/events/EventManager.c \
	main/ardev/events/EventSource.c \
	main/ardev/events/EventType.c \
	main/ardev/tasks/CallbackTask.c \
	main/ardev/tasks/CounterTaskService.c \
	main/ardev/tasks/Task.c \
	main/ardev/tasks/TaskScheduler.c \
	main/ardev/tasks/TaskService.c \
	main/ardev/ticks/Clock.c \
	main/ardev/ticks/CounterClock.c \
	main/ardev/ticks/CounterTickSource.c \
	main/ardev/ticks/TickEventType.c

libardev_a_CPPFLAGS = \
	-I$(top_srcdir)/ardev/main





TESTS = \
	AllTests

check_PROGRAMS = \
	AllTests

AllTests_SOURCES = \
	test/AllTests.cpp \
	test/ardev/events/EventManagerTest.cpp \
	test/ardev/events/TestEventListener.c \
	test/ardev/events/TestEventSource.c \
	test/ardev/tasks/CallbackTaskTest.cpp \
	test/ardev/tasks/TaskSchedulerTest.cpp \
	test/ardev/tasks/TaskServiceTest.cpp \
	test/ardev/tasks/TestClock.c \
	test/ardev/tasks/TestTask.c \
	test/ardev/ticks/TickEventTypeTest.cpp

AllTests_CPPFLAGS = \
	-I$(top_srcdir)/ardev/main \
	-I$(top_srcdir)/ardev/test \
	-I$(CPPUTEST)/include

AllTests_LDADD = \
	-L$(top_builddir)/ardev \
	-lardev \
	-L$(CPPUTEST)/lib \
	-lCppUTest

