/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <stddef.h>

#include <ardev/events/atmega328p/Atmega328pEventManager.h>





static EventManager  _eventManagerData;
static EventManager *_eventManager = NULL;





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

EventManager *Atmega328pEventManager_get() {

    if ( NULL == _eventManager ) {
        _eventManager = EventManager_init(&_eventManagerData);
    }

    return _eventManager;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void Atmega328pEventManager_start() {

    EventManager *eventManager = Atmega328pEventManager_get();

    EventManager_start(eventManager);
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/


