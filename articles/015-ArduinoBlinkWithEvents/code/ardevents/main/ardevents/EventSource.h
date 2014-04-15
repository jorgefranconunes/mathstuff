/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#ifndef EVENTSOURCE_H
#define EVENTSOURCE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <ardevents/Event.h>





    typedef struct EventSourceStruct EventSource;

    typedef struct EventSourceInterfaceStruct EventSourceInterface;
    struct EventSourceInterfaceStruct {
        Event *(*pollEvent)(EventSource *);
    };

    struct EventSourceStruct {
        EventSourceInterface *vtable;
    };





    Event *EventSource_pollEvent(EventSource *self);





#ifdef __cplusplus
}
#endif

#endif
