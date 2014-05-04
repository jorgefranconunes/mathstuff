/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <stdbool.h>
#include <stddef.h>

#include <avr/interrupt.h>

#include <ardev/events/atmega328p/Atmega328pEventManager.h>
#include <ardev/ticks/CounterTickSource.h>





#define INCREMENT_FACTOR_N 1
#define INCREMENT_FACTOR_D 1

static long getTickCount(void);
static void setupAtmega328pTimer(void);

static void init(void);

static long volatile _tickCount = 0;

static bool               _needsInit = true;
static CounterTickSource  _tickSourceData;
static CounterTickSource *_tickSource = NULL;
static Clock             *_clock      = NULL;





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

Clock *Atmega328pTickSource_getClock(void) {

    if ( _needsInit ) {
        init();
    }

    return _clock;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

EventSource *Atmega328pTickSource_getTickSource() {

    if ( _needsInit ) {
        init();
    }

    EventSource *result = CounterTickSource_asEventSource(_tickSource);

    return result;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static void init() {

    if ( !_needsInit ) {
        return;
    }

    CounterTickSource *tickSource =
        CounterTickSource_init(&_tickSourceData,
                               &getTickCount,
                               INCREMENT_FACTOR_N,
                               INCREMENT_FACTOR_D);
    Clock *clock =
        CounterTickSource_getClock(tickSource);

    EventManager *eventManager =
        Atmega328pEventManager_get();

    EventManager_addSource(eventManager,
                           CounterTickSource_asEventSource(tickSource));

    setupAtmega328pTimer();

    _tickSource = tickSource;
    _clock      = clock;
    _needsInit  = false;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static long getTickCount(void) {

    return 0; // TBD

    long result;

    uint8_t currSREG = SREG;
    cli();

    /**
     ** Start of exclusive block.
     **/

    result = _tickCount;

    /**
     ** End of exclusive block.
     **/

    SREG = currSREG;

    return result;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static void setupAtmega328pTimer() {

    return; // TBD

    // Set the Timer 0 Mode to CTC
    TCCR0A |= _BV(WGM01);

    // Set the value that you want to count to.
    OCR0A = 0xF9;

    //Set the ISR COMPA vect.
    TIMSK0 |= _BV(OCIE0A);

    // Set prescaler to 64 and start the timer.
    TCCR0B |= _BV(CS00);

    sei();
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

ISR (TIMER0_COMPA_vect) {

    ++_tickCount;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

