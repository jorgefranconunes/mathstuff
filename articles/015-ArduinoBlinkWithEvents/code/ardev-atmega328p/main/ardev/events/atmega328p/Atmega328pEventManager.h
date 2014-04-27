/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#ifndef ATMEGA328PEVENTMANAGER_H
#define ATMEGA328PEVENTMANAGER_H

#ifdef __clplusplus
extern "C" {
#endif

#include <ardev/events/EventManager.h>





    EventManager *Atmega328pEventManager_get(void);

    void Atmega328pEventManager_start();





#ifdef __cplusplus
}
#endif

#endif
