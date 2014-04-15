/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <ardevents/Event.h>





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

Event *Event_init(Event     *self,
                  EventType *eventType) {

    self->eventType = eventType;

    return self;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

EventType *Event_getEventType(Event *self) {

    return self->eventType;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

