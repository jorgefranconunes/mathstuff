/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <stddef.h>

#include <ardev/tasks/TaskScheduler.h>






/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void TaskScheduler_init(TaskScheduler *self,
                        Clock         *clock) {

    self->clock        = clock;
    self->taskListHead = NULL;
}





/**************************************************************************
 *
 * The Task will be in use until the Task is run by a call to the
 * TaskScheduler_runPendingTasks method.
 *
 * This method can be called from within a task being run by
 * TaskScheduler_runPendingTasks.
 *
 **************************************************************************/

void TaskScheduler_addTask(TaskScheduler *self,
                           Task          *task,
                           long           delay) {

    TaskScheduler_addPeriodicTask(self, task, delay, 0);
}






/**************************************************************************
 *
 * The Task will be in use until the given task is cancelled.
 *
 * This method can be called from within a task being run by
 * TaskScheduler_runPendingTasks.
 *
 **************************************************************************/

void TaskScheduler_addPeriodicTask(TaskScheduler *self,
                                   Task          *task,
                                   long           delay,
                                   long           period) {

    long now  = Clock_currentTimeMillis(self->clock);
    long when = now+delay;

    task->when   = when;
    task->period = period;
    task->status = ACTIVE;


    /* Insert the task at the right position in the list, keeping the
       list ordered by increasing time. */

    Task **ptask = &self->taskListHead;

    while ( NULL!=(*ptask) && (*ptask)->when<when ) {
        ptask = &(*ptask)->next;
    }
    task->next = *ptask;
    *ptask = task;
}






/**************************************************************************
 *
 * The Task is guaranteed to be no longer in use by the TaskScheduler
 * by the time this method returns.
 *
 * This method can be called from within a task being run by
 * TaskScheduler_runPendingTasks.
 *
 **************************************************************************/

void TaskScheduler_cancelTask(TaskScheduler *self,
                              Task          *task) {

    task->status = CANCELED;

    Task **ptask = &self->taskListHead;

    while ( NULL!=(*ptask) && (*ptask)!=task ) {
        ptask = &(*ptask)->next;
    }

    if ( NULL != (*ptask) ) {
        *ptask = task->next;
        task->next = NULL;
    } else {
        /* The given task is not in our list... Ignore it or boom?... */
    }
}





/**************************************************************************
 *
 * Do not call this method from within a Task. Otherwise bad things
 * will happen to you.
 *
 **************************************************************************/

void TaskScheduler_runPendingTasks(TaskScheduler *self) {

    /* TBD */
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

