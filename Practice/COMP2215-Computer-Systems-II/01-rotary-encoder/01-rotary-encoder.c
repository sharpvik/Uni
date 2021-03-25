/* COMP2215: Task 01---MODEL ANSWER */

/* For La Fortuna board

   | Port | Pin | Function                  |
   |------+-----+---------------------------|
   | B    |   7 | Green LED                 |
   | E    |   4 | Rotary Encoder A          |
   | E    |   5 | Rotary Encoder B          |

*/

#include <stdio.h>
#include <avr/io.h>
#include <util/delay.h>
#include <avr/interrupt.h>


#define STEP_DELAY_MS 5
#define MIN_STEP    2    /* > 0 */
#define MAX_STEP  255

void init(void);

int8_t enc_delta(void);
volatile int8_t delta;


void main(void) {
    uint8_t cnt = MAX_STEP/2;
    uint8_t i;
    int16_t res;


    init();
    sei();
    for (;;) {
        for (i=cnt; i > 0; --i) {
           _delay_ms(STEP_DELAY_MS);
           res = cnt + enc_delta();
           if (res > MAX_STEP) {
               cnt = MAX_STEP;
           } else if (res < MIN_STEP) {
               cnt = MIN_STEP;
           } else {
               cnt = res;
           }
        }
        PINB |= _BV(PINB7);   /* toggle LED */
    }
}



/* Configure I/O Ports */
void init(void) {

    /* 8MHz clock, no prescaling (DS, p. 48) */
    CLKPR = (1 << CLKPCE);
    CLKPR = 0;

    /* Configure I/O Ports */

    DDRB  |=  _BV(PB7);   /* LED pin out */
    PORTB &= ~_BV(PB7);   /* LED off */


    DDRE &= ~_BV(PE4) & ~_BV(PE5);  /* Rot. Encoder inputs */
    PORTE |= _BV(PE4) | _BV(PE5);   /* Rot. Encoder pull-ups */


    /* Timer 0 for switch scan interrupt: */

    TCCR0A = _BV(WGM01);  /* CTC Mode, DS Table 14-7 */
    TCCR0B = _BV(CS01)
           | _BV(CS00);   /* Prescaler: F_CPU / 64, DS Table 14-8 */

    /* 1 ms for manual movement of rotary encoder: */
    /* 1 ms --> 1000 Hz, Formula for CTC mode from  DS 14.6.2  */
    /* Note that the formula gives the toggle frequency, which is half the interrupt frequency. */

    OCR0A = (uint8_t)(F_CPU / (64.0 * 1000) - 1);

    TIMSK0 |= _BV(OCIE0A);  /* Enable timer interrupt, DS 14.8.6  */
}



 ISR( TIMER0_COMPA_vect ) {
     static int8_t last;
     int8_t new, diff;
     uint8_t wheel;


     /*
        Scan rotary encoder
        ===================
        This is adapted from Peter Dannegger's code available at:
        http://www.mikrocontroller.net/articles/Drehgeber
     */

     wheel = PINE;
     new = 0;
     if( wheel  & _BV(PE4) ) new = 3;
     if( wheel  & _BV(PE5) )
     new ^= 1;                  /* convert gray to binary */
     diff = last - new;         /* difference last - new  */
     if( diff & 1 ){            /* bit 0 = value (1) */
         last = new;                /* store new as next last  */
         delta += (diff & 2) - 1;   /* bit 1 = direction (+/-) */
     }

}


/* read two step encoder */
int8_t enc_delta() {
    int8_t val;

    cli();
    val = delta;
    delta &= 1;
    sei();

    return val >> 1;
}
