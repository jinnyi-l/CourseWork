#include <stdio.h>
#include <string.h>
#include "contiki.h"
#include "net/netstack.h"
#include "net/nullnet/nullnet.h"
#include "dev/leds.h"
#include "sys/etimer.h"

/* -------- 强制信道 = 26 -------- */
#undef IEEE802154_CONF_DEFAULT_CHANNEL
#define IEEE802154_CONF_DEFAULT_CHANNEL 26

PROCESS(task2_basestation_process, "Task2 Basestation: LED1=accel, LED2=button, LED3=both");
AUTOSTART_PROCESSES(&task2_basestation_process);

/* 过去 10 秒内的事件 */
static clock_time_t last_accel_time  = 0;
static clock_time_t last_button_time = 0;

/* 10 秒窗口 */
#define ACTIVE_LIMIT  (10 * CLOCK_SECOND)

/* 每 1s 刷新 LED 状态一次 */
#define CHECK_INTERVAL (CLOCK_SECOND)

/*---------------------------------------------------------------------------*/
/* 收包回调：payload=1 => accel, 2 => button */
static void
input_callback(const void *data, uint16_t len,
               const linkaddr_t *src, const linkaddr_t *dest)
{
  if(len > 0) {
    uint8_t sensor_id = ((uint8_t *)data)[0];
    clock_time_t now = clock_time();

    if(sensor_id == 1) {
      last_accel_time = now;
      printf("[Base] ACCEL from %u.%u\n", src->u8[0], src->u8[1]);
    } else if(sensor_id == 2) {
      last_button_time = now;
      printf("[Base] BUTTON from %u.%u\n", src->u8[0], src->u8[1]);
    }
  }
}

/*---------------------------------------------------------------------------*/
/* 每秒检查是否还在 10 秒窗口 => 决定各LED亮灭 */
static void
update_leds(void)
{
  clock_time_t now = clock_time();
  int accel_active  = (now - last_accel_time  < ACTIVE_LIMIT);
  int button_active = (now - last_button_time < ACTIVE_LIMIT);

  /* LED1 => 加速度 */
  if(accel_active) {
    leds_on(LEDS_RED);
  } else {
    leds_off(LEDS_RED);
  }

  /* LED2 => 按钮 */
  if(button_active) {
    leds_on(LEDS_GREEN);
  } else {
    leds_off(LEDS_GREEN);
  }

  /* LED3 => 若加速度 & 按钮都在10秒内触发 => 双重报警 */
  if(accel_active && button_active) {
    leds_on(LEDS_BLUE);
  } else {
    leds_off(LEDS_BLUE);
  }
}

/*---------------------------------------------------------------------------*/
PROCESS_THREAD(task2_basestation_process, ev, data)
{
  static struct etimer periodic_timer;

  PROCESS_BEGIN();

  printf("Task2 Basestation started\n");
  printf(" LED1=accel(RED), LED2=button(GREEN), LED3=both(BLUE)\n");

  /* 设置 NullNet 回调 */
  nullnet_set_input_callback(input_callback);

  /* 全部熄灭 */
  leds_off(LEDS_ALL);

  /* 定时器：每1秒运行 update_leds() */
  etimer_set(&periodic_timer, CHECK_INTERVAL);

  while(1) {
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&periodic_timer));
    etimer_reset(&periodic_timer);

    update_leds();
  }

  PROCESS_END();
}

