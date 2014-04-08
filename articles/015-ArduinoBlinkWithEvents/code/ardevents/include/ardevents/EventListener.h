/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#ifndef EVENTLISTENER_H
#define EVENTLISTENER_H

#ifdef __cplusplus
extern "C" {
#endif





    struct EventListenerStruct {
    };
    typedef struct EventListenerStruct EventListener;

    int  EventListener_getEventType(EventListener *self);
    void EventListener_handleEvent(EventListener *self);





#ifdef __cplusplus
}
#endif

#endif
