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





    typedef struct EventListenerStruct EventListener;

    typedef struct EventListenerInterfaceStruct EventListenerInterface;
    struct EventListenerInterfaceStruct {
        void (*notify)(EventListener*, Event*);
    };

    struct EventListenerStruct {
        EventListenerInterface *vtable;
    };

    void EventListener_notify(EventListener *self,
                              Event         *event);





#ifdef __cplusplus
}
#endif

#endif
