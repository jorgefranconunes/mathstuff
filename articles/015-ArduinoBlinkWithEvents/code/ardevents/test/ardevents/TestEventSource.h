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
        int        *detailList;
        int         detailCount;
    };
    typedef struct TestEventSourceStruct TestEventSource;

    void TestEventSource_init(TestEventSource *self,
                              int              eventType);




#ifdef __cplusplus
}
#endif

#endif

