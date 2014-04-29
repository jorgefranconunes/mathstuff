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


    typedef struct TaskServiceInterfaceStruct TaskServiceInterface;
    struct TaskServiceInterfaceStruct {
        void (*start)(TaskService *self);
    };


    struct TaskServiceStruct {
        TaskServiceInterface   *vtable;
        EventManager           *eventManager;
        TaskScheduler           scheduler;
        TaskServiceTickListener tickListener;
        EventListenerSlot       tickListenerSlot;
    };

    TaskService *TaskService_init(TaskService  *self,
                                  EventManager *eventManager,
                                  Clock        *clock);

    void TaskService_start(TaskService *self);

    void TaskService_addTask(TaskService *self,
                             Task        *task,
                             long         delay);

    void TaskService_addPeriodicTask(TaskService *self,
                                     Task        *task,
                                     long         delay,
                                     long         period);

    void TaskService_cancelTask(TaskService *self,
                                Task        *task);




#ifdef __cplusplus
}
#endif

#endif
