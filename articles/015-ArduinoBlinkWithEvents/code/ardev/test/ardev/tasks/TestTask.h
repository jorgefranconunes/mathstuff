/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#ifndef TESTTASK_H
#define TESTTASK_H

#ifdef __cplusplus
extern "C" {
#endif

#include <ardev/tasks/Task.h>





    typedef struct TestTaskStruct TestTask;

    struct TestTaskStruct {
        Task base;
        int  callCount;
    };

    void TestTask_init(TestTask *self);

    Task *TestTask_asTask(TestTask *self);

    int TestTask_getCallCount(TestTask *self);





#ifdef __cplusplus
}
#endif

#endif
