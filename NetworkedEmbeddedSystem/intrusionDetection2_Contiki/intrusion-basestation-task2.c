#include <stdio.h>
#include <string.h>
#include "contiki.h"
#include "dev/leds.h"
#include "sys/etimer.h"
#include "net/netstack.h"
#include "net/nullnet/nullnet.h"
#include "net/linkaddr.h"


//timestamp the events happened in past 10 sec, if no event lights off

static clock_time_t last_accel_time  = 0;
static clock_time_t last_button_time = 0;


#define ACTIVE_LIMIT  (10 * CLOCK_SECOND)

#define CHECK_INTERVAL (CLOCK_SECOND)



//callback of NullNet
/*
data: Pointer to the received data.
len: Length of the received data.
src: Sender's network address.
dest: Receiver's network address.

read sensor_id, update timestamp o current timestamp
*/
static void input_callback(const void *data, uint16_t len,
                           const linkaddr_t *src, const linkaddr_t *dest) {
  if (len > 0) {
    uint8_t sensor_id = ((uint8_t *)data)[0];
    clock_time_t now = clock_time();

    if (sensor_id == 1) {
      last_accel_time = now;
      printf("[Base] ACCEL from %d.%d\n", src->u8[0], src->u8[1]);  // 修正 src->u8 访问方式
    } else if (sensor_id == 2) {
      last_button_time = now;
      printf("[Base] BUTTON from %d.%d\n", src->u8[0], src->u8[1]);
    }
  }
}



/*
Setup light rule: 

if the event happens within the 10s window:

If the last accelerometer event happened within the last 10 seconds, accel_active = 1.
If the last button event happened within the last 10 seconds, button_active = 1.

*/
static void update_leds(void) {
  clock_time_t now = clock_time();
  int accel_active  = (now - last_accel_time  < ACTIVE_LIMIT);
  int button_active = (now - last_button_time < ACTIVE_LIMIT);


  //initially all lights off
  uint8_t led_state = 0b0000;


  if (accel_active) {
    led_state |= 0b0001; // LED1 (P0.13)
  }
  if (button_active) {
    led_state |= 0b0010; // LED2 (P0.14)
  }
  if (accel_active && button_active) {
    led_state |= 0b0100; // LED3 (P0.15)
  }

  leds_off(0b1111); 
  leds_on(led_state);
}





//define and auto start process

PROCESS(task2_basestation_process, "Task2 Basestation");
AUTOSTART_PROCESSES(&task2_basestation_process);





PROCESS_THREAD(task2_basestation_process, ev, data) {


  static struct etimer periodic_timer; //declare timer and start process
  PROCESS_BEGIN();


  nullnet_set_input_callback(input_callback); //register packet reception handler

  etimer_set(&periodic_timer, CHECK_INTERVAL);

  while (1) {
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&periodic_timer));
    etimer_reset(&periodic_timer);

    update_leds();
  }

  PROCESS_END();
}
