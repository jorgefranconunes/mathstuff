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

#include <ardevents/Event.h>





    struct EventListenerStruct;
    typedef struct EventListenerStruct EventListener;

    struct EventListenerInterfaceStruct {
        void (*notify)(EventListener*, Event*);
    };
    typedef struct EventListenerInterfaceStruct EventListenerInterface;

    struct EventListenerStruct {
        EventListenerInterface *vtable;
    };

    void EventListener_notify(EventListener *self,
                              Event         *event);





#ifdef __cplusplus
}
#endif

#endif
