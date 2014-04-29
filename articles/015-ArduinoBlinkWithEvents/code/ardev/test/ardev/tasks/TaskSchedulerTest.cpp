/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <CppUTest/TestHarness.h>

#include <ardev/tasks/TaskScheduler.h>

#include <ardev/tasks/TestClock.h>





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

TEST_GROUP(TaskScheduler) {


    TestClock      testClockData;
    TestClock     *testClock;
    TaskScheduler  schedulerData;
    TaskScheduler *scheduler;


    void setup() {

        TestClock_init(&testClockData);
        testClock = &testClockData;

        TaskScheduler_init(&schedulerData, TestClock_asClock(testClock));
        scheduler = &schedulerData;
    }


    void teardown() {
    }

};





TEST(TaskScheduler, doInit) {

    // Everything is handled in the testsuit setup.

    CHECK_EQUAL(0, TestClock_time(testClock));
}





TEST(TaskScheduler, checkTestClock) {

    CHECK_EQUAL(0, TestClock_time(testClock));
    TestClock_setTime(testClock, 1234);
    CHECK_EQUAL(1234, TestClock_time(testClock));

    Clock *clock = TestClock_asClock(testClock);
    CHECK_EQUAL(1234, Clock_currentTimeMillis(clock));
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

