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

#include <stdbool.h>





struct EventSourceStruct;
typedef struct EventSourceStruct *EventSource;

int  EventSource_getEventType(EventSource self);
bool EventSource_isPending(EventSource self);





#ifdef __cplusplus
}
#endif

#endif
