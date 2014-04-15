/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#ifndef EVENTTYPE_H
#define EVENTTYPE_H

#ifdef __cplusplus
extern "C" {
#endif





    struct EventTypeStruct {
        int id;
    };
    typedef struct EventTypeStruct EventType;





    EventType *EventType_init(EventType *);
    int        EventType_getId(EventType *);





#ifdef __cplusplus
}
#endif

#endif
