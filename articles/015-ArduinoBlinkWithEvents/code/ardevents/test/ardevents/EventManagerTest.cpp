/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <CppUTest/TestHarness.h>

#include <ardevents/EventManager.h>
#include <ardevents/EventSource.h>

#include <ardevents/TestEventListener.h>
#include <ardevents/TestEventSource.h>





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

TEST_GROUP(EventManagerBasics) {


    EventManager eventManager;


    void setup() {

        EventManager_init(&eventManager);
    }


    void teardown() {
    }





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

    void testMultiEvents(int             sourceCount,
                         TestEventSource sourceList[],
                         EventSourceSlot sourceSlotList[],
                         int             sourceEventTypeList[],
                         int            *tickCountListList[],
                         int             tickCountSizeList[],
                         int             expectedQueryCount,

                         int               listenerCount,
                         TestEventListener listenerList[],
                         EventListenerSlot listenerSlotList[],
                         int               listenerEventTypeList[],
                         int               maxEventCountList[],
                         int               expectedEventCount[]) {

        for ( int i=0; i<sourceCount; ++i ) {
            TestEventSource *rSource = &sourceList[i];
            TestEventSource_init(rSource,
                                 sourceEventTypeList[i],
                                 tickCountListList[i],
                                 tickCountSizeList[i]);
            EventManager_addSource(&eventManager,
                                   &sourceSlotList[i],
                                   TestEventSource_asEventSource(rSource));
        }

        for ( int i=0; i<listenerCount; ++i ) {
            TestEventListener *rLstnr = &listenerList[i];
            TestEventListener_init(rLstnr,
                                   listenerEventTypeList[i],
                                   maxEventCountList[i],
                                   &eventManager);
            EventManager_addListener(&eventManager,
                                     &listenerSlotList[i],
                                     TestEventListener_asEventListener(rLstnr));
        }

        EventManager_start(&eventManager);

        for ( int i=0; i<sourceCount; ++i ) {
            TestEventSource *rSource = &sourceList[i];
            CHECK_EQUAL(expectedQueryCount,
                        TestEventSource_getQueryCount(rSource));
        }

        for ( int i=0; i<listenerCount; ++i ) {
            TestEventListener *rLstnr = &listenerList[i];
            CHECK_EQUAL(expectedEventCount[i],
                        TestEventListener_getEventCount(rLstnr));
        }
    }





/**************************************************************************
 *
 * Test one source with one listener.
 *
 **************************************************************************/

    void test1x1(int tickCountList[],
                 int tickCountSize,
                 int maxEventCount,
                 int expectedQueryCount,
                 int expectedEventCount) {

        int eventTypeList[] = { 101 };

        TestEventSource sourceList[1];
        EventSourceSlot sourceSlotList[1];
        int            *tickCountListList[] = { tickCountList };
        int             tickCountSizeList[] = { tickCountSize };

        TestEventListener listenerList[1];
        EventListenerSlot listenerSlotList[1];
        int               maxEventCountList[] = { maxEventCount };
        int               expectedEventCountList[] = { expectedEventCount };

        testMultiEvents(1,
                        sourceList,
                        sourceSlotList,
                        eventTypeList,
                        tickCountListList,
                        tickCountSizeList,
                        expectedQueryCount,
                        1,
                        listenerList,
                        listenerSlotList,
                        eventTypeList,
                        maxEventCountList,
                        expectedEventCountList);
    }





/**************************************************************************
 *
 * Test two sources with one listener.
 *
 **************************************************************************/

    void test2x1(int  sourceEventTypeList[],
                 int *tickCountListList[],
                 int  tickCountSizeList[],
                 int  expectedQueryCount,
                 int  listenerEventType,
                 int  maxEventCount,
                 int  expectedEventCount) {

        TestEventSource sourceList[2];
        EventSourceSlot sourceSlotList[2];

        TestEventListener listenerList[1];
        EventListenerSlot listenerSlotList[1];
        int               listenerEventTypeList[] = { listenerEventType };
        int               maxEventCountList[] = { maxEventCount };
        int               expectedEventCountList[] = { expectedEventCount };

        testMultiEvents(2,
                        sourceList,
                        sourceSlotList,
                        sourceEventTypeList,
                        tickCountListList,
                        tickCountSizeList,
                        expectedQueryCount,
                        1,
                        listenerList,
                        listenerSlotList,
                        listenerEventTypeList,
                        maxEventCountList,
                        expectedEventCountList);
    }


};





TEST(EventManagerBasics, doInit) {

    // Everything is handled in the testsuit setup.
}





TEST(EventManagerBasics, startWithNothingToDo) {

    EventManager_start(&eventManager);
}





TEST(EventManagerBasics, startWithNoListener) {

    TestEventSource source;
    EventSourceSlot sourceSlot;

    TestEventSource_init(&source, 100, NULL, 0);

    EventManager_addSource(&eventManager,
                           &sourceSlot,
                           TestEventSource_asEventSource(&source));
    EventManager_start(&eventManager);

    CHECK_EQUAL(0, TestEventSource_getQueryCount(&source));
}





TEST(EventManagerBasics, startWithNoSource) {

    TestEventListener listener;
    EventListenerSlot listenerSlot;

    TestEventListener_init(&listener, 100, 1, &eventManager);

    EventManager_addListener(&eventManager,
                             &listenerSlot,
                             TestEventListener_asEventListener(&listener));
    EventManager_start(&eventManager);

    CHECK_EQUAL(0, TestEventListener_getEventCount(&listener));
}





TEST(EventManagerBasics, oneEvent) {

    int tickCountList[] = { 1 };

    test1x1(tickCountList, 1, 1, 1, 1);
}





TEST(EventManagerBasics, multipleEvents01) {

    int tickCountList[] = { 10, 0 };
    test1x1(tickCountList, 2, 10, 10, 10);
}





TEST(EventManagerBasics, multipleEvents02) {

    int tickCountList[] = { 10, 0, 10 };
    test1x1(tickCountList, 3, 15, 15, 15);
}





TEST(EventManagerBasics, multipleEvents03) {

    int tickCountList[] = { 10, 4, 10 };
    test1x1(tickCountList, 3, 15, 19, 15);
}





TEST(EventManagerBasics, multipleEvents04) {

    int tickCountList[] = { 0, 10, 4, 10 };
    test1x1(tickCountList, 4, 3, 13, 3);
}





TEST(EventManagerBasics, twoSources01) {

    int  sourceEventTypeList[] = { 100, 101 };
    int  tickCountList01[] = {2, 3};
    int  tickCountList02[] = {4, 5};
    int *tickCountListList[] = { tickCountList01, tickCountList02 };
    int  tickCountSizeList[] = { 2, 2 };
    int  expectedQueryCount = 3;
    int  listenerEventType  = 101;
    int  maxEventCount      = 3;
    int  expectedEventCount = 3;

    test2x1(sourceEventTypeList,
            tickCountListList,
            tickCountSizeList,
            expectedQueryCount,
            listenerEventType,
            maxEventCount,
            expectedEventCount);
}





TEST(EventManagerBasics, twoSources02) {

    int  sourceEventTypeList[] = { 101, 101 };
    int  tickCountList01[] = {2, 3};
    int  tickCountList02[] = {4, 5};
    int *tickCountListList[] = { tickCountList01, tickCountList02 };
    int  tickCountSizeList[] = { 2, 2 };
    int  expectedQueryCount = 3;
    int  listenerEventType  = 101;
    int  maxEventCount      = 5;
    int  expectedEventCount = 5;

    test2x1(sourceEventTypeList,
            tickCountListList,
            tickCountSizeList,
            expectedQueryCount,
            listenerEventType,
            maxEventCount,
            expectedEventCount);
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

