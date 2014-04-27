/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <ardev/ticks/CounterClock.h>





static long CounterClock_currentTimeMillis(Clock *self);

static ClockInterface interface = {
    .currentTimeMillis = CounterClock_currentTimeMillis
};





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void CounterClock_init(CounterClock *self,
                       int           incrementFactorN,
                       int           incrementFactorD) {

    self->base.vtable = &interface;
    self->incrFactorN = incrementFactorN;
    self->incrFactorD = incrementFactorD;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void CounterClock_update(CounterClock *self,
                         long          currentTickCount) {

    // TBD
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

Clock *CounterClock_asClock(CounterClock *self) {

    Clock *result = (Clock *)self;

    return result;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static long CounterClock_currentTimeMillis(Clock *self) {

    CounterClock *me     = (CounterClock *)self;
    long          result = me->currentTimeMillis;

    return result;

}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

