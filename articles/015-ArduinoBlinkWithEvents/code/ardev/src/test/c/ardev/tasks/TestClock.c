/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <ardev/tasks/TestClock.h>





static long TestClock_getCurrentTimeMillis(Clock *baseSelf);

static ClockInterface interface = {
    .currentTimeMillis = &TestClock_getCurrentTimeMillis
};





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void TestClock_init(TestClock *self) {

    self->base.vtable = &interface;
    self->time        = 0;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void TestClock_setTime(TestClock *self,
                       long       time) {

    self->time = time;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

void TestClock_addTime(TestClock *self,
                       long       interval) {

    self->time += interval;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

long TestClock_time(TestClock *self) {

    long result = self->time;

    return result;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

Clock *TestClock_asClock(TestClock *self) {

    Clock *result = (Clock *)self;

    return result;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

static long TestClock_getCurrentTimeMillis(Clock *baseSelf) {

    TestClock *self   = (TestClock *)baseSelf;
    long       result = TestClock_time(self);

    return result;

}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

