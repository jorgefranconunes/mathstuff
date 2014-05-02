/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <stddef.h>

#include <ardev/events/atmega328p/Atmega328pEventManager.h>
#include <ardev/ticks/atmega328p/Atmega328pTickSource.h>
#include <ardev/tasks/CounterTaskService.h>





static TaskService  _taskServiceData;
static TaskService *_taskService = NULL;





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

TaskService *Atmega328pTaskService_get () {

    if ( NULL == _taskService ) {
        EventManager *eventManager = Atmega328pEventManager_get();
        Clock         clock        = Atmega328pTickSource_getClock();
        TaskService *taskService   =
            TaskService_init(&_taskServiceData, eventManager, clock);

        TaskService_start(taskService);

        _taskService = taskService;
    }

    return _taskService;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

