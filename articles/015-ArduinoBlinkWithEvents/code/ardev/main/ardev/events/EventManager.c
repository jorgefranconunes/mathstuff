/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <stdbool.h>
#include <stddef.h>

#include <ardev/events/Event.h>
#include <ardev/events/EventManager.h>





static void EventSourceSlot_init(EventSourceSlot *self,
                                 EventSource     *source,
                                 EventSourceSlot *next);

static void EventListenerSlot_init(EventListenerSlot *self,
                                   EventType         *eventType,
                                   EventListener     *listener,
                                   EventListenerSlot *next);

static void EventManager_checkEventSources(EventManager *self);
static void EventManager_doEventLoop(EventManager *self);
static void EventManager_fireEvent(EventManager *self,
                                   Event        *event);
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
                              EventType         *eventType,
                              EventListener     *eventListener) {

    EventListenerSlot_init(slot,
                           eventType,
                           eventListener,
                           self->eventListenerListHead);

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
        Event       *event       = EventSource_pollEvent(eventSource);

        if ( NULL != event ) {
            EventManager_fireEvent(self, event);
        }
    }
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static void EventManager_fireEvent(EventManager *self,
                                   Event        *event) {

    EventType         *eventType    = Event_getEventType(event);
    EventListenerSlot *listenerSlot = NULL;

    for ( listenerSlot=self->eventListenerListHead;
          listenerSlot!=NULL;
          listenerSlot=listenerSlot->next ) {

        EventListener *listener     = listenerSlot->item;
        EventType     *listenerType = listenerSlot->eventType;
        bool           isInterested = (eventType==listenerType);

        if ( isInterested ) {
            EventListener_notify(listener, event);
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
                                   EventType         *eventType,
                                   EventListener     *listener,
                                   EventListenerSlot *next) {

    self->eventType = eventType;
    self->item      = listener;
    self->next      = next;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

