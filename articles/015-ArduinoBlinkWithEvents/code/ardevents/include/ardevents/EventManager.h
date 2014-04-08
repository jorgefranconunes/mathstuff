/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#ifndef EVENTMANAGER_H
#define EVENTMANAGER_H

#ifdef __cplusplus
extern "C" {
#endif

#include <ardevents/EventListener.h>
#include <ardevents/EventSource.h>





    struct EventSourceSlotStruct {
        EventSource      item;
        EventSourceSlot *next;
    };
    typedef struct EventSourceSlotStruct *EventSourceSlot;

    struct EventListenerSlotStruct {
        EventListener      item;
        EventListenerSlot *next;
    };
    typedef struct EventListenerSlotStruct *EventListenerSlot;


    struct EventManagerStruct;
    typedef struct EventManagerStruct *EventManager;

    void EventManager_init(EventManager self);

    void EventManager_addSource(EventManager    self,
                                EventSourceSlot slot,
                                EventSource     eventSource);

    void EventManager_addListener(EventManager      self,
                                  EventListenerSlot slot,
                                  EventListener     eventListener);

    void EventManager_start(EventManager self);

    void EventManager_stop(EventManager self);





#ifdef __cplusplus
}
#endif

#endif
