/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <ardevents/TestEventListener.h>





static int  TestEventListener_getEventType(EventListener *self);
static void TestEventListener_handleEvent(EventListener *self);

static EventListenerInterface interface = {
    .getEventType = TestEventListener_getEventType,
    .handleEvent  = TestEventListener_handleEvent
};





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void TestEventListener_init(TestEventListener *self,
                            int                eventType,
                            int                maxEventCount,
                            EventManager      *eventManager) {

    self->base.vtable   = &interface;
    self->eventType     = eventType;
    self->maxEventCount = maxEventCount;
    self->eventManager  = eventManager;
    self->eventCount    = 0;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

EventListener *TestEventListener_asEventListener(TestEventListener *self) {

    EventListener *result = (EventListener *)self;

    return result;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

int TestEventListener_getEventCount(TestEventListener *self) {

    TestEventListener *me = (TestEventListener *)self;

    int result = me->eventCount;

    return result;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static int TestEventListener_getEventType(EventListener *self) {

    TestEventListener *me = (TestEventListener *)self;

    int result = me->eventType;

    return result;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static void TestEventListener_handleEvent(EventListener *self) {

    TestEventListener *me = (TestEventListener *)self;

    if ( me->eventCount < me->maxEventCount ) {
        ++(me->eventCount);

        if ( me->eventCount == me->maxEventCount ) {
            EventManager_stop(me->eventManager);
        }
    }
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

