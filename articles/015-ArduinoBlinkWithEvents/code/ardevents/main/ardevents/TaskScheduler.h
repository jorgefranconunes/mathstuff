/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#ifndef TASKSCHEDULER_H
#define TASKSCHEDULER_H

#ifdef __cplusplus
extern "C" {
#endif





    typedef struct TaskSlotStruct taskSlot;
    struct TaskSlotStruct {
        Task     *item;
        long      when;
        TaskSlot *next;
    };


    typedef struct TaskSchedulerStruct TaskScheduler;

    typedef struct TaskSchedulerInterfaceStruct TaskSchedulerInterface;
    struct TaskSchedulerInterfaceStruct {
        TaskScheduler *(*addTask)(TaskScheduler *scheduler,
                                  TaskSlot      *taskSlot,
                                  Task          *task,
                                  long           delay);
        TaskSchduler *(*addPeriodicTask)(TaskScheduler *scheduler,
                                         TaskSlot      *taskSlot,
                                         Task          *task,
                                         long           period);
    };

    struct TaskSchedulerStruct {
        TaskSchedulerInterface *vtable;
    };





    TaskScheduler *TaskScheduler_addTask(TaskScheduler *scheduler,
                                         TaskSlot      *taskSlot,
                                         Task          *task,
                                         long           delay);

    TaskScheduler *TaskScheduler_addPeriodicTask(TaskScheduler *scheduler,
                                                 TaskSlot      *taskSlot,
                                                 Task          *task,
                                                 long           period);




#ifdef __cplusplus
}
#endif

#endif
