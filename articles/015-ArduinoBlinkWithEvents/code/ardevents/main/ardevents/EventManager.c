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

static void EventManager_checkEventSources(EventManager *self);
static void EventManager_doEventLoop(EventManager *self);
static void EventManager_fireEvent(EventManager *self,
                                   int           eventType);
static bool EventManager_isRunning(EventManager *self);





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void EventManager_init(EventManager *self) {

    self->status                = STOPED;
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

    if ( !EventManager_isRunning(self) ) {
        bool hasStuffToDo =
            (self->eventSourceListHead!=NULL)
            && (self->eventListenerListHead!=NULL);

        if ( hasStuffToDo ) {
            self->status = STARTED;
            EventManager_doEventLoop(self);
        } else {
            /* Either we have no events sources or we have no one to
               send events to. So we have nothing to do. */
        }
    } else {
        /* We are already running. We should blow out...*/
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

static void EventManager_doEventLoop(EventManager *self) {

    while ( EventManager_isRunning(self) ) {
        EventManager_checkEventSources(self);
    }
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static void EventManager_checkEventSources(EventManager *self) {

    EventSourceSlot *sourceSlot = NULL;
    
    for ( sourceSlot=self->eventSourceListHead;
          sourceSlot!=NULL;
          sourceSlot=sourceSlot->next ) {

        EventSource *eventSource = sourceSlot->item;

        if ( EventSource_isPending(eventSource) ) {
            int eventType = EventSource_getEventType(eventSource);

            EventManager_fireEvent(self, eventType);
        }
    }
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static void EventManager_fireEvent(EventManager *self,
                                   int           eventType) {

    EventListenerSlot *listenerSlot = NULL;

    for ( listenerSlot=self->eventListenerListHead;
          listenerSlot!=NULL;
          listenerSlot=listenerSlot->next ) {

        EventListener *listener     = listenerSlot->item;
        int            listenerType = EventListener_getEventType(listener);
        bool           isInterested = (eventType==listenerType);

        if ( isInterested ) {
            EventListener_handleEvent(listener);
        }
    }
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void EventManager_stop(EventManager *self) {

    self->status = STOPED;
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

