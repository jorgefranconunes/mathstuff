/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <ardev/events/EventSource.h>





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

Event *EventSource_poll(EventSource *self) {

    return self->vtable->poll(self);
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

