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

#include <ardevents/EventListener.h>
#include <ardevents/EventManager.h>





    struct TestEventListenerStruct {
        EventListener base;
        int           eventType;
        int           maxEventCount;
        EventManager *eventManager;
        int           eventCount;
    };
    typedef struct TestEventListenerStruct TestEventListener;

    void TestEventListener_init(TestEventListener *self,
                                int                eventType,
                                int                maxEventCount,
                                EventManager      *eventManager);

    EventListener *TestEventListener_asEventListener(TestEventListener *self);
    int            TestEventListener_getEventCount(TestEventListener *self);





#ifdef __cplusplus
}
#endif

#endif

