/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <ardevents/TestEventSource.h>





static int  TestEventSource_getEventType(EventSource *self);
static bool TestEventSource_isPending(EventSource *self);

static EventSourceInterface interface = {
    .getEventType = TestEventSource_getEventType,
    .isPending    = TestEventSource_isPending
};





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void TestEventSource_init(TestEventSource *self,
                          int              eventType) {

    self->base.vtable = &interface;
    self->eventType   = eventType;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static int TestEventSource_getEventType(EventSource *self) {

    TestEventSource *me = (TestEventSource *)self;

    /* TBD... */
    return 0;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static bool TestEventSource_isPending(EventSource *self) {

    TestEventSource *me = (TestEventSource *)self;

    /* TBD... */
    return false;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

