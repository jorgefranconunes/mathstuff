/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#ifndef TASKSERVICE_H
#define TASKSERVICE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <ardev/events/EventListener.h>
#include <ardev/events/EventManager.h>
#include <ardev/tasks/Task.h>
#include <ardev/tasks/TaskScheduler.h>
#include <ardev/ticks/Clock.h>




    typedef struct TaskServiceStruct TaskService;


    typedef struct TaskServiceTickListenerStruct TaskServiceTickListener;
    struct TaskServiceTickListenerStruct {
        EventListener base;
        TaskService  *taskService;
    };





    struct TaskServiceStruct {
        EventManager           *eventManager;
        TaskScheduler           scheduler;
        TaskServiceTickListener tickListener;
        EventListenerSlot       tickListenerSlot;
    };

    TaskService *TaskService_init(TaskService  *self,
                                  EventManager *eventManager,
                                  Clock        *clock);

    TaskService *TaskService_start(TaskService *self);

    TaskService *TaskService_addTask(TaskService *self,
                                     TaskSlot    *taskSlot,
                                     Task        *task,
                                     long         delay);

    TaskService *TaskService_addPeriodicTask(TaskService *self,
                                             TaskSlot    *taskSlot,
                                             Task        *task,
                                             long         delay,
                                             long         period);

    TaskService *TaskService_cancelTask(TaskService *self,
                                        TaskSlot    *taskSlot);




#ifdef __cplusplus
}
#endif

#endif
