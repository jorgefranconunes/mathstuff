/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#ifndef TESTEVENTSOURCE_H
#define TESTEVENTSOURCE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <ardevents/EventSource.h>





    struct TestEventSourceStruct {
        EventSource base;
        int         eventType;
        int        *tickCountList;
        int         tickCountSize;
        int         currentTick;
        int         currentRemaining;
        bool        isActive;
        int         queryCount;
    };
    typedef struct TestEventSourceStruct TestEventSource;

    void TestEventSource_init(TestEventSource *self,
                              int              eventType,
                              int             *tickCountList,
                              int              tickCountSize);

    EventSource *TestEventSource_asEventSource(TestEventSource *self);
    int          TestEventSource_getQueryCount(TestEventSource *self);





#ifdef __cplusplus
}
#endif

#endif

