/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#ifndef SYSTICKSOURCE_H
#define SYSTICKSOURCE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <ardev/events/EventSource.h>
#include <ardev/ticks/Clock.h>





    void SysTickSource_init(long (*tickCountGetter)(void),
                            int    incrementFactorN,
                            int    incrementFactorD);

    Clock *SysTickSource_getClock(void);

    EventSource *SysTickSource_getTickSource(void);





#ifdef __cplusplus
}
#endif

#endif
