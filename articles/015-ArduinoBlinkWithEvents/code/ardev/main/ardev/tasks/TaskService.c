/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <stddef.h>

#include <ardev/tasks/TaskService.h>
#include <ardev/ticks/TickEventType.h>





static EventListener *
TaskServiceTickListener_asEventListener(TaskServiceTickListener *self);

static void
TaskServiceTickListener_init(TaskServiceTickListener *self,
                             TaskService              *taskService);

static void
TaskServiceTickListener_notify(EventListener *self,
                               Event         *Event);

static EventListenerInterface taskServiceEventListenerInterface = {
    .notify = TaskServiceTickListener_notify
};






/**************************************************************************
 *
 * 
 *
 **************************************************************************/

TaskService *TaskService_init(TaskService  *self,
                              EventManager *eventManager,
                              Clock        *clock) {

    self->eventManager = eventManager;
    TaskScheduler_init(&self->scheduler, clock);
    TaskServiceTickListener_init(&self->tickListener, self);

    return self;
}






/**************************************************************************
 *
 * 
 *
 **************************************************************************/

TaskService *TaskService_start(TaskService  *self) {

    EventListener *tickListener =
        TaskServiceTickListener_asEventListener(&self->tickListener);

    EventManager_addListener(self->eventManager,
                             &self->tickListenerSlot,
                             TickEventType_get(),
                             tickListener);

    return self;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

TaskService *TaskService_addTask(TaskService *self,
                                 TaskSlot    *taskSlot,
                                 Task        *task,
                                 long         delay) {

    TaskScheduler_addTask(&self->scheduler, taskSlot, task, delay);

    return self;
}






/**************************************************************************
 *
 * The TaskSlot will be in use until the given task is cancelled.
 *
 * This method can be called from within a task being run by
 * TaskService_runPendingTasks.
 *
 **************************************************************************/

TaskService *TaskService_addPeriodicTask(TaskService *self,
                                         TaskSlot    *taskSlot,
                                         Task        *task,
                                         long         delay,
                                         long         period) {

    TaskScheduler_addPeriodicTask(&self->scheduler,
                                  taskSlot,
                                  task,
                                  delay,
                                  period);

    return self;
}






/**************************************************************************
 *
 * The TaskSlot is guaranteed to be no longer in use by the
 * TaskService by the time this method returns.
 *
 * This method can be called from within a task being run by
 * TaskService_runPendingTasks.
 *
 **************************************************************************/

TaskService *TaskService_cancelTask(TaskService *self,
                                    TaskSlot    *taskSlot) {

    TaskScheduler_cancelTask(&self->scheduler, taskSlot);

    return self;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void TaskService_tickEvent(TaskService *self) {

    TaskScheduler_runPendingTasks(&self->scheduler);
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static void
TaskServiceTickListener_init(TaskServiceTickListener *self,
                             TaskService              *taskService) {

    self->base.vtable = &taskServiceEventListenerInterface;
    self->taskService = taskService;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

EventListener *
TaskServiceTickListener_asEventListener(TaskServiceTickListener *self) {

    EventListener *result = (EventListener *)self;

    return result;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static void TaskServiceTickListener_notify(EventListener *self,
                                            Event         *Event) {

    TaskServiceTickListener *me = (TaskServiceTickListener *)self;

    TaskService_tickEvent(me->taskService);
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

