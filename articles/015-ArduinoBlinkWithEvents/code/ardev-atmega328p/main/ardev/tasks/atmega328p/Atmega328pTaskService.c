/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <stddef.h>

#include <avr/interrupt.h>

#include <ardev/events/atmega328p/Atmega328pEventManager.h>
#include <ardev/tasks/CounterTaskService.h>





#define INCREMENT_FACTOR_N 1
#define INCREMENT_FACTOR_D 1

static long getTickCount(void);
static void setupAtmega328pTimer(void);

static long volatile _tickCount = 0;

static CounterTaskService  _taskServiceData;
static CounterTaskService *_taskService = NULL;





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

TaskService *Atmega328pTaskService_get () {

    if ( NULL == _taskService ) {
        CounterTaskServiceConf conf = {
            .eventManager     = Atmega328pEventManager_get();
            .tickCountIniter  = &setupAtmega328pTimer;
            .tickCountGetter  = &getTickCount;
            .incrementFactorN = INCREMENT_FACTOR_N;
            .incrementFactorD = INCREMENT_FACTOR_D;
        };

        CounterTaskService_init(&_taskServiceData, &conf);
        _taskService = &_taskServiceData;

        TaskService_start(CounterTaskService_asTaskService(_taskService));
    }

    TaskService result = CounterTaskService_asTaskService(_taskService);

    return result;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static long getTickCount() {

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

    SREG = currSReg;

    return result;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static void setupAtmega328pTimer() {

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

