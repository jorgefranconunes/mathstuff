/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <avr/interrupt.h>

#include <ardev/sys/SysTickSource.h>





#define INCREMENT_FACTOR_N 1
#define INCREMENT_FACTOR_D 1

static long getTickCount(void);
static void setupAtmega328pTimer(void);

static bool _isInited = false;
static long volatile _tickCount = 0;





/**************************************************************************
 *
 * Initializes the system tick source. Only after calling this
 * function you can safely call <code>SysTickSource_get()</code>
 *
 * It is ok to call this function more than once. Only the first call
 * will have an effect.
 *
 **************************************************************************/

void Atm328pTickSource_init() {

    if ( !_isInited ) {
        SysTickSource_init(&getTickCount,
                           INCREMENT_FACTOR_N,
                           INCREMENT_FACTOR_D);
        setupAtmega328pTimer();

        _isInited = true;
    }
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

