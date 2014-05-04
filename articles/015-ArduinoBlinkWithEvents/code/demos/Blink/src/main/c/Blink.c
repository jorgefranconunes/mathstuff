/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <stdbool.h>

#include <avr/io.h>

#include <ardev/tasks/CallbackTask.h>

#include <ardev/tasks/atmega328p/Atmega328pTaskService.h>
#include <ardev/events/atmega328p/Atmega328pEventManager.h>





#define BLINK_DELAY_MS 500

static void blinkCallback(void);





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

int main(void) {

    /* Set pin 5 of PORTD for output*/
    DDRD |= _BV(DDD5);


    CallbackTask callbackTaskData;
    Task        *task = CallbackTask_build(&callbackTaskData, &blinkCallback);
    TaskService *taskService = Atmega328pTaskService_get();
    
    TaskService_addPeriodicTask(taskService, task, 0, BLINK_DELAY_MS);

    /* Run forever. */
    Atmega328pEventManager_start();
}





/**************************************************************************
 *
 * Called once per 0.5s. Toggles the LED connected to port 5.
 *
 **************************************************************************/

static void blinkCallback() {

    static bool isOn = false;

    if ( isOn ) {
        PORTD |= _BV(PORTD5);
    } else {
        PORTD &= ~_BV(PORTD5);
    }

    isOn = !isOn;
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

