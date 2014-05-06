/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#ifndef SYSEVENTMANAGER_H
#define SYSEVENTMANAGER_H

#ifdef __clplusplus
extern "C" {
#endif

#include <ardev/events/EventManager.h>





    EventManager *SysEventManager_get(void);

    void SysEventManager_start();





#ifdef __cplusplus
}
#endif

#endif
