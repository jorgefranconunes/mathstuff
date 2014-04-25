/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <stddef.h>

#include <ardevents/TaskScheduler.h>






/**************************************************************************
 *
 * 
 *
 **************************************************************************/

TaskScheduler *TaskScheduler_init(TaskScheduler *self,
                                  Clock         *clock) {

    self->clock        = clock;
    self->taskListHead = NULL;

    return self;
}





/**************************************************************************
 *
 * The TaskSlot will be in use until the Task is run by a call to the
 * TaskScheduler_runPendingTasks method.
 *
 * This method can be called from within a task being run by
 * TaskScheduler_runPendingTasks.
 *
 **************************************************************************/

TaskScheduler *TaskScheduler_addTask(TaskScheduler *self,
                                     TaskSlot      *taskSlot,
                                     Task          *task,
                                     long           delay) {

    TaskScheduler *result =
        TaskScheduler_addPeriodicTask(self, taskSlot, task, delay, 0);

    return result;
}






/**************************************************************************
 *
 * The TaskSlot will be in use until the given task is cancelled.
 *
 * This method can be called from within a task being run by
 * TaskScheduler_runPendingTasks.
 *
 **************************************************************************/

TaskScheduler *TaskScheduler_addPeriodicTask(TaskScheduler *self,
                                             TaskSlot      *taskSlot,
                                             Task          *task,
                                             long           delay,
                                             long           period) {

    long now  = Clock_currentTimeMillis(self->clock);
    long when = now+delay;

    taskSlot->task   = task;
    taskSlot->when   = when;
    taskSlot->period = period;
    taskSlot->status = ACTIVE;


    /* Insert the task slot at the right position in the list, keeping
       the list ordered by increasing time. */

    TaskSlot **ptask = &self->taskListHead;

    while ( NULL!=(*ptask) && (*ptask)->when<when ) {
        ptask = &(*ptask)->next;
    }
    taskSlot->next = *ptask;
    *ptask = taskSlot;

    return self;
}






/**************************************************************************
 *
 * The TaskSlot is guaranteed to be no longer in use by the
 * TaskScheduler by the time this method returns.
 *
 * This method can be called from within a task being run by
 * TaskScheduler_runPendingTasks.
 *
 **************************************************************************/

TaskScheduler *TaskScheduler_cancelTask(TaskScheduler *self,
                                        TaskSlot      *taskSlot) {

    taskSlot->status = CANCELED;

    TaskSlot **ptask = &self->taskListHead;

    while ( NULL!=(*ptask) && (*ptask)!=taskSlot ) {
        ptask = &(*ptask)->next;
    }

    if ( NULL != (*ptask) ) {
        *ptask = taskSlot->next;
        taskSlot->next = NULL;
    } else {
        /* The given task is not in our list... Ignore it or boom?... */
    }

    return self;
}





/**************************************************************************
 *
 * Do not call this method from within a Task. Otherwise bad things
 * will happen to you.
 *
 **************************************************************************/

TaskScheduler *TaskScheduler_runPendingTasks(TaskScheduler *self) {

    /* TBD */

    return self;
}






/**************************************************************************
 *
 * 
 *
 **************************************************************************/

