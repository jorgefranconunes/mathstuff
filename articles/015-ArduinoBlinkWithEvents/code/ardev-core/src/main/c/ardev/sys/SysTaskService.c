/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <stdbool.h>
#include <stddef.h>

#include <ardev/sys/SysEventManager.h>
#include <ardev/sys/SysTickSource.h>
#include <ardev/tasks/CounterTaskService.h>





static void init(void);

static bool         _needsInit = true;
static TaskService  _taskServiceData;
static TaskService *_taskService = NULL;





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

TaskService *SysTaskService_get () {

    if ( _needsInit ) {
        init();
    }

    return _taskService;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static void init() {

    if ( !_needsInit ) {
        return;
    }

    EventManager *eventManager = SysEventManager_get();
    Clock        *clock        = SysTickSource_getClock();
    TaskService  *taskService  =
            TaskService_init(&_taskServiceData, eventManager, clock);

    TaskService_start(taskService);

    _taskService = taskService;
    _needsInit   = false;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

