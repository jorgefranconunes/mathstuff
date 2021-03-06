/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <stdbool.h>

#include <avr/io.h>

#include <ardev/tasks/CallbackTask.h>

#include <ardev/sys/SysEventManager.h>
#include <ardev/sys/SysTaskService.h>
#include <ardev/sys/atm328p/Atm328pTaskService.h>





#define BLINK_DELAY_MS 1000L

static void blinkCallback(void);

static bool _isLedOn = true;

static CallbackTask callbackTaskData;
static Task        *task;





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

int main(void) {

    Atm328pTaskService_init();

    /* Set pin 5 of PORTD for output*/
    DDRD |= _BV(DDD5);

    TaskService *taskService = SysTaskService_get();
    
    task = CallbackTask_build(&callbackTaskData, &blinkCallback);
    TaskService_addPeriodicTask(taskService, task, 0L, BLINK_DELAY_MS);

    /* Run forever. */
    SysEventManager_start();
}





/**************************************************************************
 *
 * Called once per 0.5s. Toggles the LED connected to port 5.
 *
 **************************************************************************/

static void blinkCallback() {

    if ( _isLedOn ) {
        PORTD |= _BV(PORTD5);
    } else {
        PORTD &= ~_BV(PORTD5);
    }

    _isLedOn = !_isLedOn;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

