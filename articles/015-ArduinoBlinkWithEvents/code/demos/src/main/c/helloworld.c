#include <avr/io.h>
#include <util/delay.h>


const int BLINK_DELAY_MS = 500;
 

int main (void) {

    /* Set pin 5 of PORTD for output*/
    DDRD |= _BV(DDD5);
 
    while(1) {
        /* Set pin 5 high to turn led on */
        PORTD |= _BV(PORTD5);
        _delay_ms(BLINK_DELAY_MS);
 
        /* Set pin 5 low to turn led off */
        PORTD &= ~_BV(PORTD5);
        _delay_ms(BLINK_DELAY_MS);
    }
 
    return 0;
}

