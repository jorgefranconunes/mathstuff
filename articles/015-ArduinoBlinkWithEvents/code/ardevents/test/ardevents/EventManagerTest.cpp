/**************************************************************************
 *
 * Copyright (c) 2014 Jorge Nunes, All Rights Reserved.
 *
 **************************************************************************/

#include <CppUTest/TestHarness.h>

#include <ardevents/EventManager.h>





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

TEST_GROUP(EventManager) {


    void setup() {
    }


    void teardown() {
    }
};





TEST(EventManager, doInit) {

    EventManager eventManager;

    EventManager_init(&eventManager);
}





TEST(EventManager, startWithNothingToDo) {

    EventManager eventManager;

    EventManager_init(&eventManager);
    EventManager_start(&eventManager);
}





/**************************************************************************
 *
 * 
 *
 **************************************************************************/

