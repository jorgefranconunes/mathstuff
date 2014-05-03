/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#ifndef TESTEVENTLISTENER_H
#define TESTEVENTLISTENER_H

#ifdef __cplusplus
extern "C" {
#endif

#include <ardev/events/EventListener.h>
#include <ardev/events/EventManager.h>
#include <ardev/events/EventType.h>





    struct TestEventListenerStruct {
        EventListener base;
        EventType    *eventType;
        int           maxEventCount;
        EventManager *eventManager;
        int           eventCount;
    };
    typedef struct TestEventListenerStruct TestEventListener;

    void TestEventListener_init(TestEventListener *self,
                                EventType         *eventType,
                                int                maxEventCount,
                                EventManager      *eventManager);

    EventListener *TestEventListener_asEventListener(TestEventListener *self);

    int            TestEventListener_getEventCount(TestEventListener *self);





#ifdef __cplusplus
}
#endif

#endif

