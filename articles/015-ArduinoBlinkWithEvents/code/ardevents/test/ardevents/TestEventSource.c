/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <stddef.h>

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
                          int              eventType,
                          int             *tickCountList,
                          int              tickCountSize) {

    self->base.vtable      = &interface;
    self->eventType        = eventType;
    self->tickCountList    = tickCountList;
    self->tickCountSize    = tickCountSize;
    self->currentTick      = 0;
    self->currentRemaining = (tickCountList!=NULL) ? tickCountList[0] : 0;
    self->isActive         = (tickCountList!=NULL);
    self->queryCount       = 0;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

EventSource *TestEventSource_asEventSource(TestEventSource *self) {

    EventSource *result = (EventSource *)self;

    return result;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

int TestEventSource_getQueryCount(TestEventSource *self) {

    TestEventSource *me = (TestEventSource *)self;

    int result = me->queryCount;

    return result;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static int TestEventSource_getEventType(EventSource *self) {

    TestEventSource *me = (TestEventSource *)self;

    int result = me->eventType;

    return result;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static bool TestEventSource_isPending(EventSource *self) {

    TestEventSource *me = (TestEventSource *)self;

    while ( (me->currentTick<me->tickCountSize)
            && (me->currentRemaining==0) ) {
        ++(me->currentTick);
        if ( me->currentTick < me->tickCountSize ) {
            me->currentRemaining =
                me->tickCountList[me->currentTick];
            me->isActive = !me->isActive;
        } else {
            /* At the end! */
        }
    }

    if ( me->currentTick < me->tickCountSize ) {
        --(me->currentRemaining);
    } else {
        // We reached the end of our activity. We are no longer
        // generating events.
    }

    ++(me->queryCount);

    return me->isActive;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

