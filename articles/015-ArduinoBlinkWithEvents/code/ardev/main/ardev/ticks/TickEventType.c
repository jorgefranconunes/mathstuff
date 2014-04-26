/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <stddef.h>

#include <ardev/ticks/TickEventType.h>





static EventType  _tickEventTypeData;
static EventType *_tickEventType = NULL;





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

EventType *TickEventType_get() {

    if ( NULL == _tickEventType ) {
        _tickEventType = EventType_init(&_tickEventTypeData);
    }

    return _tickEventType;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/


