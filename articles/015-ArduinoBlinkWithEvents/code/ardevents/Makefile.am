#
#
#

AM_CFLAGS = -g -Os -Wall

noinst_LIBRARIES = \
	libardevents.a

libardevents_a_SOURCES = \
	src/ardevents/helloutils.c \
	src/ardevents/EventListener.c \
	src/ardevents/EventManager.c \
	src/ardevents/EventSource.c

libardevents_a_CPPFLAGS = \
	-I$(top_srcdir)/ardevents/include





TESTS = \
	AllTests

check_PROGRAMS = \
	AllTests

AllTests_SOURCES = \
	test/AllTests.cpp \
	test/ardevents/EventManagerTest.cpp \
	test/ardevents/TestEventSource.c

AllTests_CPPFLAGS = \
	-I$(top_srcdir)/ardevents/include \
	-I$(top_srcdir)/ardevents/test \
	-I$(CPPUTEST)/include

AllTests_LDADD = \
	-L$(top_builddir)/ardevents \
	-lardevents \
	-L$(CPPUTEST)/lib \
	-lCppUTest \
	-lCppUTestExt

