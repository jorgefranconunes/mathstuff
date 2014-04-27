/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <avr/io.h>

#include <ardev/tasks/CallbackTask.h>

#include <ardev/tasks/atm328p/Atm328pTaskService.h>
#include <ardev/events/atm328p/Atm328pEventManager.h>





define BLINK_DELAY_MS 500

static void blinkCallback(void);





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

int main(void) {

    CallbackTask callbackTaskData;
    Task        *task = CallbackTask_build(&callbackTaskData, &blinkCallback);
    TaskSlot     taskSlot;
    TaskService *taskService = Atm328pTaskService_get();
    
    TaskService_addPeriodicTask(taskService,
                                &taskSlot,
                                task,
                                0,
                                BLINK_DELAY_MS);

    /* Set pin 5 of PORTD for output*/
    DDRD |= _BV(DDD5);

    /* And run forever. */
    Atmega328pEventManager_start();
}





/**************************************************************************
 *
 * Called once per 0.5s. Toggles the LED connected to port 5.
 *
 **************************************************************************/

static void blinkCallback() {

    /* Toggle pin 5. */
    PORTD ^= _BV(PORTD5);
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

