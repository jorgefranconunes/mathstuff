/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#ifndef COUNTERTICKSOURCE_H
#define COUNTERTICKSOURCE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <ardev/events/Event.h>
#include <ardev/events/EventSource.h>
#include <ardev/ticks/CounterClock.h>





    typedef struct CounterTickSourceStruct CounterTickSource;

    struct CounterTickSourceStruct {
        EventSource  base;
        long (*tickCountGetter)(void);
        CounterClock clock;
        Event        tickEvent;
        long         lastTickCount;
    };

    CounterTickSource *
    CounterTickSource_init(CounterTickSource *self,
                           long             (*tickCountGetter)(void),
                           int                incrementFactorN,
                           int                incrementeFactorD);

    Clock *
    CounterTickSource_getClock(CounterTickSource *self);

    EventSource *
    CounterTickSource_asEventSource(CounterTickSource *self);





#ifdef __cplusplus
}
#endif

#endif
