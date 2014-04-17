/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#ifndef TASK_H
#define TASK_H

#ifdef __cplusplus
extern "C" {
#endif





    typedef struct TaskStruct Task;

    typedef struct TaskInterfaceStruct TaskInterface;
    struct TaskInterfaceStruct {
        void (*run)();
    };

    struct TaskStruct {
        TaskInterface *vtable;
    };





    void Task_run(Task *self);





#ifdef __cplusplus
}
#endif

#endif

