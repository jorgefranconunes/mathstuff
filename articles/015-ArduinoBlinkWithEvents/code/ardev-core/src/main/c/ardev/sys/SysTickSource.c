/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>

#include <ardev/sys/SysEventManager.h>
#include <ardev/ticks/CounterTickSource.h>





static bool               _needsInit = true;
static CounterTickSource  _tickSourceData;
static CounterTickSource *_tickSource = NULL;
static Clock             *_clock      = NULL;





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void SysTickSource_init(long (*tickCountGetter)(void),
                        int    incrementFactorN,
                        int    incrementFactorD) {

    CounterTickSource *tickSource =
        CounterTickSource_init(&_tickSourceData,
                               tickCountGetter,
                               incrementFactorN,
                               incrementFactorD);
    Clock *clock =
        CounterTickSource_getClock(tickSource);

    EventManager *eventManager =
        SysEventManager_get();

    EventManager_addSource(eventManager,
                           CounterTickSource_asEventSource(tickSource));

    _tickSource = tickSource;
    _clock      = clock;
    _needsInit  = false;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

EventSource *SysTickSource_get() {

    assert( _needsInit == false );

    EventSource *result = CounterTickSource_asEventSource(_tickSource);

    return result;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

Clock *SysTickSource_getClock(void) {

    assert( _needsInit == false );

    return _clock;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

