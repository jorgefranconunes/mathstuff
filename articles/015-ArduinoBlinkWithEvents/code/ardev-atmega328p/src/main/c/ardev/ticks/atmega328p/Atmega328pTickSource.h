/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#ifndef ATMEGA328PTICKSOURCE_H
#define ATMEGA328PTICKSOURCE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <ardev/ticks/Clock.h>





    Clock *Atmega328pTickSource_getClock(void);

    EventSource *Atmega328pTickSource_getTickSource(void);





#ifdef __cplusplus
}
#endif

#endif
