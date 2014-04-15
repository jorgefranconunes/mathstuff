/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <assert.h>
#include <stddef.h>

#include <ardevents/Event.h>
#include <ardevents/TestEventListener.h>





static void TestEventListener_notify(EventListener *self,
                                     Event         *event);

static EventListenerInterface interface = {
    .notify  = TestEventListener_notify
};





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void TestEventListener_init(TestEventListener *self,
                            EventType         *eventType,
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

static void TestEventListener_notify(EventListener *self,
                                     Event         *event) {

    TestEventListener *me = (TestEventListener *)self;
    EventType         *expectedEventType = me->eventType;

    if ( NULL != expectedEventType ) {
        assert( expectedEventType == Event_getEventType(event) );
    }

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

