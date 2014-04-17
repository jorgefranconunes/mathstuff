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
#include <ardevents/EventType.h>
#include <ardevents/EventSource.h>





    typedef struct EventSourceSlotStruct EventSourceSlot;
    struct EventSourceSlotStruct {
        EventSource     *item;
        EventSourceSlot *next;
    };


    typedef struct EventListenerSlotStruct EventListenerSlot;
    struct EventListenerSlotStruct {
        EventType         *eventType;
        EventListener     *item;
        EventListenerSlot *next;
    };


    typedef enum EventManagerStatus {
        STOPED,
        STARTED
    } EventManagerStatus;


    typedef struct EventManagerStruct EventManager;
    struct EventManagerStruct {
        EventManagerStatus status;
        EventSourceSlot   *eventSourceListHead;
        EventListenerSlot *eventListenerListHead;
    };


    void EventManager_init(EventManager *self);

    void EventManager_addSource(EventManager    *self,
                                EventSourceSlot *slot,
                                EventSource     *eventSource);

    void EventManager_addListener(EventManager      *self,
                                  EventListenerSlot *slot,
                                  EventType         *eventType,
                                  EventListener     *eventListener);

    void EventManager_start(EventManager *self);

    void EventManager_stop(EventManager *self);





#ifdef __cplusplus
}
#endif

#endif
