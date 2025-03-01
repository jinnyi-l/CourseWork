#include <stdio.h>
#include <string.h>
#include "contiki.h"
#include "dev/button-sensor.h"
#include "dev/leds.h"
#include "net/netstack.h"
#include "net/nullnet/nullnet.h"

#include "sys/clock.h"
#include "net/linkaddr.h"

/*---------------------------------------------------------------------------*/
PROCESS(clicker_ng_process, "Clicker NG Process");
AUTOSTART_PROCESSES(&clicker_ng_process);

#define MAX_EVENT 3
#define TIMEOUT (30 * CLOCK_SECOND)

struct event {
  clock_time_t time;
  linkaddr_t addr;
};

//update every time handle_event is called
static struct event event_history[MAX_EVENT];
/*---------------------------------------------------------------------------*/

/*---------------------------------------------------------------------------*/

static void handle_event(const linkaddr_t *src){

  clock_time_t now  = clock_time(); 
  int j, k;
  int valid_event = 0; //count the num of valid events in 30 seconds
  
  //clear up event_history
  for(j = 0; j < MAX_EVENT; j ++){
    if (now - event_history[j].time <= TIMEOUT){
      event_history[valid_event] = event_history[j]; //cp event at j to idx valid_event
      valid_event ++; 
    }
  }
    
  //the rest are not valid event, update them 
  for(k = valid_event; k < MAX_EVENT; k++){
    event_history[k].time = 0;
    event_history[k].addr.u8[0] = 0;
    event_history[k].addr.u8[1] = 0;
  }
    
  //double check if already exist in the array 
  {
    int existed = 0;
    for(j = 0; j < valid_event; j++){
      if (linkaddr_cmp(src, &event_history[j].addr)){
        event_history[j].time = now; //same event happens again NOW
        existed = 1;
        break;
      }
    }
    
    if(!existed) {
      
      //array not full, add new valid event
      if(valid_event < MAX_EVENT){
        event_history[valid_event].addr = *src;
        event_history[valid_event].time = now;
        valid_event ++;
      } else {
      
        //if the array is full, get rid of the oldest valid event
        int oldest = 0;
        for(j = 1; j < valid_event; j++){
          if(event_history[oldest].time > event_history[j].time){
            oldest = j;
          }
        }
        event_history[oldest].addr = *src;
        event_history[oldest].time = now; 
      }
    }
  }
    
  //turn on the blue light 
  if(valid_event >= 3){
    leds_on(LEDS_YELLOW);
    printf("Node %d: alarm triggered\n", linkaddr_node_addr.u8[0]);
  }else{
    leds_off(LEDS_YELLOW);
    printf("Node %d: alarm cleared\n", linkaddr_node_addr.u8[0]);
  }
    
  printf("Node %d: Valid events count: %d\n",
         linkaddr_node_addr.u8[0], valid_event);
}

/*---------------------------------------------------------------------------*/

/*

data: data received 
len: length of data received
src: address of the node that sent the message.
dest: address of the node that received the message.
*/

static void recv(const void *data, uint16_t len,
  const linkaddr_t *src, const linkaddr_t *dest) {
  
  //print sender's address
  printf("Received: %s - from %d\n", (char*) data, src->u8[0]);
  
  //add to run the function
  handle_event(src);
  
  //toggle green light
  leds_toggle(LEDS_GREEN);
}
/*---------------------------------------------------------------------------*/

static void cleanup_expired() {
  clock_time_t now = clock_time();
  int i, valid_event = 0;
  
  for(i = 0; i < MAX_EVENT; i++) {
    if(now - event_history[i].time <= TIMEOUT) {
      event_history[valid_event] = event_history[i];
      valid_event++;
    }
  }

  for(i = valid_event; i < MAX_EVENT; i++) {
    event_history[i].time = 0;
    event_history[i].addr.u8[0] = 0;
    event_history[i].addr.u8[1] = 0;
  }

  if(valid_event >= 3) {
    leds_on(LEDS_YELLOW);
    printf("Node %d: alarm triggered (cleanup)\n", linkaddr_node_addr.u8[0]);
  } else {
    leds_off(LEDS_YELLOW);
    printf("Node %d: alarm cleared (cleanup)\n", linkaddr_node_addr.u8[0]);
  }
}

/*---------------------------------------------------------------------------*/
PROCESS_THREAD(clicker_ng_process, ev, data)
{
  static char payload[] = "hej";
  static struct etimer periodic_timer;

  PROCESS_BEGIN();

  linkaddr_set_node_addr(&linkaddr_node_addr);
  printf("** My Rime address = %u.%u **\n",
         linkaddr_node_addr.u8[0], linkaddr_node_addr.u8[1]);

  memset(event_history, 0, sizeof(event_history));

  nullnet_buf = (uint8_t *)payload;
  nullnet_len = sizeof(payload);
  nullnet_set_input_callback(recv);

  SENSORS_ACTIVATE(button_sensor);

  etimer_set(&periodic_timer, 5 * CLOCK_SECOND);

  while(1) {
    PROCESS_WAIT_EVENT();
    
    /*check if two events:
    1.button being pushed
    2.trigger cleanup_expired() every five sec
    */

    if(ev == sensors_event && data == &button_sensor) {
      
      leds_toggle(LEDS_RED); 
      handle_event(&linkaddr_node_addr);
      NETSTACK_NETWORK.output(NULL);

    } else if(ev == PROCESS_EVENT_TIMER && data == &periodic_timer) {
      cleanup_expired();
      etimer_reset(&periodic_timer);
    }
  }

  PROCESS_END();
}

/*---------------------------------------------------------------------------*/
