/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <CppUTest/TestHarness.h>

#include <ardev/events/Event.h>
#include <ardev/tasks/TaskService.h>
#include <ardev/ticks/TickEventType.h>

#include <ardev/events/TestEventSource.h>
#include <ardev/tasks/TestClock.h>
#include <ardev/tasks/TestTask.h>





static int tickCountList[] = { 1000 };





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

TEST_GROUP(TaskService) {


    Event  tickEventData;
    Event *tickEvent;

    EventSourceSlot eventSourceSlotData;

    TestEventSource  tickEventSourceData;
    TestEventSource *tickEventSource;

    TestClock     testClockData;
    TestClock    *testClock;

    EventManager  eventManagerData;
    EventManager *eventManager;

    TaskService   taskServiceData;
    TaskService  *taskService;





    void setup() {

        Event_init(&tickEventData, TickEventType_get());
        tickEvent = &tickEventData;

        TestEventSource_init(&tickEventSourceData, tickEvent, tickCountList, 1);
        tickEventSource = &tickEventSourceData;

        TestClock_init(&testClockData);
        testClock = &testClockData;

        EventManager_init(&eventManagerData);
        eventManager = &eventManagerData;
        EventManager_addSource(eventManager,
                               &eventSourceSlotData,
                               TestEventSource_asEventSource(tickEventSource));

        TaskService_init(&taskServiceData,
                         eventManager,
                         TestClock_asClock(testClock));
        taskService = &taskServiceData;
        TaskService_start(taskService);
    }





    void teardown() {
    }

};





TEST(TaskService, doInit) {

    CHECK_EQUAL(0, TestClock_time(testClock));
}





TEST(TaskService, addOneTask) {

    TestTask testTask;
    TestTask_init(&testTask);

    TaskService_addTask(taskService, TestTask_asTask(&testTask), 10);
}





TEST(TaskService, addTaskNoRun) {

    TestTask testTask;
    TestTask_init(&testTask);

    TaskService_addTask(taskService, TestTask_asTask(&testTask), 10);
    CHECK_EQUAL(0, TestTask_getCallCount(&testTask));

    EventManager_sweep(eventManager);
    CHECK_EQUAL(0, TestTask_getCallCount(&testTask));
}





TEST(TaskService, addTaskRunOnTime) {

    TestTask testTask;
    TestTask_init(&testTask);

    TaskService_addTask(taskService, TestTask_asTask(&testTask), 10);

    TestClock_addTime(testClock, 10);

    EventManager_sweep(eventManager);
    CHECK_EQUAL(1, TestTask_getCallCount(&testTask));

    EventManager_sweep(eventManager);
    CHECK_EQUAL(1, TestTask_getCallCount(&testTask));
}





TEST(TaskService, addTaskRunAfterTime) {

    TestTask testTask;
    TestTask_init(&testTask);

    TaskService_addTask(taskService, TestTask_asTask(&testTask), 10);

    TestClock_addTime(testClock, 15);

    EventManager_sweep(eventManager);
    CHECK_EQUAL(1, TestTask_getCallCount(&testTask));

    TestClock_addTime(testClock, 15);

    EventManager_sweep(eventManager);
    CHECK_EQUAL(1, TestTask_getCallCount(&testTask));
}





TEST(TaskService, cancelTask) {

    TestTask testTask;
    TestTask_init(&testTask);

    TaskService_addTask(taskService, TestTask_asTask(&testTask), 10);
    TaskService_cancelTask(taskService, TestTask_asTask(&testTask));

    TestClock_addTime(testClock, 15);
    EventManager_sweep(eventManager);

    CHECK_EQUAL(0, TestTask_getCallCount(&testTask));
}





TEST(TaskService, addPeriodicTask) {

    TestTask testTask;
    TestTask_init(&testTask);

    TaskService_addPeriodicTask(taskService,
                                TestTask_asTask(&testTask),
                                5,
                                10);

    EventManager_sweep(eventManager);
    CHECK_EQUAL(0, TestTask_getCallCount(&testTask));

    TestClock_addTime(testClock, 10);

    EventManager_sweep(eventManager);
    CHECK_EQUAL(1, TestTask_getCallCount(&testTask));

    TestClock_addTime(testClock, 10);

    EventManager_sweep(eventManager);
    CHECK_EQUAL(2, TestTask_getCallCount(&testTask));
}





TEST(TaskService, cancelPeriodicTask) {

    TestTask testTask;
    TestTask_init(&testTask);

    TaskService_addPeriodicTask(taskService,
                                TestTask_asTask(&testTask),
                                5,
                                10);

    EventManager_sweep(eventManager);
    CHECK_EQUAL(0, TestTask_getCallCount(&testTask));

    TestClock_addTime(testClock, 10);

    EventManager_sweep(eventManager);
    CHECK_EQUAL(1, TestTask_getCallCount(&testTask));

    TestClock_addTime(testClock, 10);

    TaskService_cancelTask(taskService, TestTask_asTask(&testTask));

    EventManager_sweep(eventManager);
    CHECK_EQUAL(1, TestTask_getCallCount(&testTask));
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

