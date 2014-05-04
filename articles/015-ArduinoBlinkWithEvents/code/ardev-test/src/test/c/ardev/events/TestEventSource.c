/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <stdbool.h>
#include <stddef.h>

#include <ardev/events/TestEventSource.h>





static Event *TestEventSource_pollEvent(EventSource *self);

static EventSourceInterface interface = {
    .pollEvent    = TestEventSource_pollEvent
};





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

TestEventSource *TestEventSource_init(TestEventSource *self,
                                      Event           *event,
                                      int             *tickCountList,
                                      int              tickCountSize) {

    self->base.vtable      = &interface;
    self->event            = event;
    self->tickCountList    = tickCountList;
    self->tickCountSize    = tickCountSize;
    self->currentTick      = 0;
    self->currentRemaining = (tickCountList!=NULL) ? tickCountList[0] : 0;
    self->isActive         = (tickCountList!=NULL);
    self->queryCount       = 0;

    return self;
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

static Event *TestEventSource_pollEvent(EventSource *self) {

    TestEventSource *me     = (TestEventSource *)self;
    Event           *result = NULL;

    while ( (me->currentTick<me->tickCountSize)
            && (0==me->currentRemaining) ) {
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

    if ( me->isActive ) {
        result = me->event;
    }

    return result;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

