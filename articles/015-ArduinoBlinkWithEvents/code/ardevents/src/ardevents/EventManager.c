/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <stddef.h>

#include <ardevents/EventManager.h>





static void EventSourceSlot_init(EventSourceSlot *self,
                                 EventSource     *source,
                                 EventSourceSlot *next);

static void EventListenerSlot_init(EventListenerSlot *self,
                                   EventListener     *listener,
                                   EventListenerSlot *next);

static bool EventManager_isRunning(EventManager *self);
static void EventManager_checkEventSources(EventManager *self);





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void EventManager_init(EventManager *self) {

    self->eventSourceListHead   = NULL;
    self->eventListenerListHead = NULL;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void EventManager_addSource(EventManager    *self,
                            EventSourceSlot *slot,
                            EventSource     *eventSource) {

    EventSourceSlot_init(slot, eventSource, self->eventSourceListHead);

    self->eventSourceListHead = slot;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void EventManager_addListener(EventManager      *self,
                              EventListenerSlot *slot,
                              EventListener     *eventListener) {

    EventListenerSlot_init(slot, eventListener, self->eventListenerListHead);

    self->eventListenerListHead = slot;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void EventManager_start(EventManager *self) {

    while ( EventManager_isRunning(self) ) {
        EventManager_checkEventSources(self);
    }
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static bool EventManager_isRunning(EventManager *self) {

    bool isRunning = (self->status == STARTED);

    return isRunning;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static void EventManager_checkEventSources(EventManager *self) {

    /* TBD */
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void EventManager_stop(EventManager *self) {

    self->status = STARTED;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static void EventSourceSlot_init(EventSourceSlot *self,
                                 EventSource     *source,
                                 EventSourceSlot *next) {

    self->item = source;
    self->next = next;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static void EventListenerSlot_init(EventListenerSlot *self,
                                   EventListener     *listener,
                                   EventListenerSlot *next) {

    self->item = listener;
    self->next = next;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

