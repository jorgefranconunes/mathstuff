/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#ifndef EVENT_H
#define EVENT_H

#ifdef __cplusplus
extern "C" {
#endif

#include <ardevents/EventType.h>





    struct EventStruct {
        EventType *eventType;
    };
    typedef struct EventStruct Event;





    Event     *Event_init(Event     *event,
                          EventType *eventType);
    EventType *Event_getEventType(Event *event);





#ifdef __cplusplus
}
#endif

#endif
