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

#include <ardevents/Clock.h>
#include <ardevents/Task.h>




    typedef enum TaskStatus {
        ACTIVE,
        CANCELED,
        DONE,
        RESCHEDULE
    } TaskStatus;


    typedef struct TaskSlotStruct TaskSlot;
    struct TaskSlotStruct {
        Task      *task;
        long       when;
        long       period;
        TaskStatus status;
        TaskSlot  *next;
    };


    typedef struct TaskSchedulerStruct TaskScheduler;
    struct TaskSchedulerStruct {
        Clock    *clock;
        TaskSlot *taskListHead;
    };





    TaskScheduler *TaskScheduler_init(TaskScheduler *self,
                                      Clock         *clock);

    TaskScheduler *TaskScheduler_addTask(TaskScheduler *self,
                                         TaskSlot      *taskSlot,
                                         Task          *task,
                                         long           delay);

    TaskScheduler *TaskScheduler_addPeriodicTask(TaskScheduler *self,
                                                 TaskSlot      *taskSlot,
                                                 Task          *task,
                                                 long           delay,
                                                 long           period);

    TaskScheduler *TaskScheduler_cancelTask(TaskScheduler *self,
                                            TaskSlot      *taskSlot);

    TaskScheduler *TaskScheduler_runPendingTasks();




#ifdef __cplusplus
}
#endif

#endif
