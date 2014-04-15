#
#
#

AM_CFLAGS = -g -Os -Wall

noinst_LIBRARIES = \
	libardevents.a

libardevents_a_SOURCES = \
	main/helloutils.c \
	main/ardevents/Event.c \
	main/ardevents/EventListener.c \
	main/ardevents/EventManager.c \
	main/ardevents/EventSource.c \
	main/ardevents/EventType.c \
	main/ardevents/TickEventType.c

libardevents_a_CPPFLAGS = \
	-I$(top_srcdir)/ardevents/main





TESTS = \
	AllTests

check_PROGRAMS = \
	AllTests

AllTests_SOURCES = \
	test/AllTests.cpp \
	test/ardevents/EventManagerTest.cpp \
	test/ardevents/TestEventListener.c \
	test/ardevents/TestEventSource.c \
	test/ardevents/TickEventTypeTest.cpp

AllTests_CPPFLAGS = \
	-I$(top_srcdir)/ardevents/main \
	-I$(top_srcdir)/ardevents/test \
	-I$(CPPUTEST)/include

AllTests_LDADD = \
	-L$(top_builddir)/ardevents \
	-lardevents \
	-L$(CPPUTEST)/lib \
	-lCppUTest

