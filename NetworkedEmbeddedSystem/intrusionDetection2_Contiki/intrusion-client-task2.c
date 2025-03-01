#include <stdio.h>
#include <string.h>
#include "contiki.h"
#include "dev/adxl345.h"         //interface of accelerometer
#include "dev/button-sensor.h"
#include "net/netstack.h"
#include "net/nullnet/nullnet.h" 

#undef IEEE802154_CONF_DEFAULT_CHANNEL
#define IEEE802154_CONF_DEFAULT_CHANNEL 26


//define contiki process, and automate it with macro
PROCESS(client_process, "Task2 Client (Z1) : Accel + Button");
AUTOSTART_PROCESSES(&client_process);

//declare event timer 
static struct etimer accel_timer;

//motion threshold
static int16_t threshold = 50;


//identifier length 1 : 1 = acceleration, 2 = button
static uint8_t payload[1];




static int16_t baseline_x;
static int16_t current_x;
static int16_t diff;



static void
recv_cb(const void *data, uint16_t len,
        const linkaddr_t *src, const linkaddr_t *dest)
{
  /* no op */
}


PROCESS_THREAD(client_process, ev, data)
{
  PROCESS_BEGIN();


  
  accm_init();
  

  /* activate button monitor */
  SENSORS_ACTIVATE(button_sensor);

  //only on x-axis
  baseline_x = accm_read_axis(X_AXIS);
  printf("[Z1] baseline_x=%d\n", baseline_x);

  
  //assign payload as data buffer fo NullNet
  nullnet_buf = payload;
  nullnet_len = sizeof(payload);
  nullnet_set_input_callback(recv_cb);

  //100hz: 10ms = 0.1s
  etimer_set(&accel_timer, CLOCK_SECOND / 10);


  while(1) {
  
  /* Wait until an event occurs. If the event has
		 * occured, ev will hold the type of event, and
		 * data will have additional information for the
		 * event. In the case of a sensors_event, data will
		 * point to the sensor that caused the event.
		 * Here we wait until the button was pressed. */
  
    //first in waiting state
    PROCESS_WAIT_EVENT();
    
    //1 happens: accelerometer triggers

    if(ev == PROCESS_EVENT_TIMER && etimer_expired(&accel_timer)) {
      etimer_reset(&accel_timer);

      current_x = accm_read_axis(X_AXIS);
      diff = current_x - baseline_x;

      printf("[Z1] X=%d diff=%d\n", current_x, diff);

      /* exceed threshold, change payload to 1 indicating acceleration event happens  */
      
      if(diff > threshold || diff < -threshold) {
        payload[0] = 1;  // ACCEL
        NETSTACK_NETWORK.output(NULL);
        printf("[Z1] Sent ACCEL event (1)\n");
      }
    }
    

    
    if(ev == sensors_event && data == &button_sensor) {
      payload[0] = 2;  // BUTTON
      NETSTACK_NETWORK.output(NULL);
      printf("[Z1] Sent BUTTON event (2)\n");
    }
  }

  PROCESS_END();
}
