/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#ifndef COUNTERTASKSERVICE_H
#define COUNTERTASKSERVICE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <ardev/tasks/TaskService.h>
#include <ardev/ticks/CounterTickSource.h>





    typedef struct CounterTaskServiceConfStruct CounterTaskServiceConf;

    struct CounterTaskServiceConfStruct {
        EventManager *eventManager;
        void        (*tickCountIniter)(void);
        long        (*tickCountGetter)(void);
        int           incrementFactorN;
        int           incrementFactorD;
    };


    typedef struct CounterTaskServiceStruct CounterTaskService;

    struct CounterTaskServiceStruct {
        TaskService       base;
        CounterTickSource tickSource;
        EventSourceSlot   tickSourceSlot;
        EventManager     *eventManager;
        void            (*tickCountIniter)(void);
    };

    void CounterTaskService_init(CounterTaskService     *self,
                                 CounterTaskServiceConf *conf);

    TaskService *CounterTaskService_asTaskService(CounterTaskService *self);




#ifdef __cplusplus
}
#endif

#endif
