/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <CppUTest/TestHarness.h>

#include <ardev/sys/SysTickSource.h>





int   _counterValueIndex    = 0;
long *_counterValueList     = NULL;
int   _maxCounterValueIndex = 0;


static void initCounter(long *counterValueList,
                        int   counterValuesSize) {

    _counterValueIndex    = 0;
    _counterValueList     = counterValueList;
    _maxCounterValueIndex = counterValuesSize - 1;
}


static long getCounter() {

    long result = _counterValueList[_counterValueIndex];

    if ( _counterValueIndex < _maxCounterValueIndex ) {
        ++_counterValueIndex;
    }

    return result;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

TEST_GROUP(SysTickSource) {


    


    void setup() {
    }


    void teardown() {
    }


};





TEST(SysTickSource, noEvents) {

    long counterValueList[] = { 0 };
    int  counterValuesSize  = 1;
    initCounter(counterValueList, counterValuesSize);

    SysTickSource_init(&getCounter, 1 , 1);

    EventSource *eventSource = SysTickSource_get();
    Event       *event       = EventSource_poll(eventSource);

    POINTERS_EQUAL(NULL, event);
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

