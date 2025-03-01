#include <stdio.h>
#include "contiki.h"
#include "sys/etimer.h"
#include "nrfx_gpiote.h"

/* 物理 LED GPIO 定义 */
#define LED1_PIN  13  // 对应 P0.13
#define LED2_PIN  14  // 对应 P0.14
#define LED3_PIN  15  // 对应 P0.15
#define LED4_PIN  16  // 对应 P0.16

PROCESS(test_gpio_leds, "Test GPIO LEDs");
AUTOSTART_PROCESSES(&test_gpio_leds);

/* 设置 GPIO 方向为输出 */
void init_gpio_leds() {
  nrfx_gpiote_init();
  nrfx_gpiote_out_config_t config = NRFX_GPIOTE_CONFIG_OUT_SIMPLE(false);
  
  nrfx_gpiote_out_init(LED1_PIN, &config);
  nrfx_gpiote_out_init(LED2_PIN, &config);
  nrfx_gpiote_out_init(LED3_PIN, &config);
  nrfx_gpiote_out_init(LED4_PIN, &config);
}

PROCESS_THREAD(test_gpio_leds, ev, data) {
  static struct etimer timer;
  PROCESS_BEGIN();

  init_gpio_leds();  // 初始化 GPIO

  printf("=== GPIO LED Test Started ===\n");

  while(1) {
    printf("Turning ON LED1 (P0.13)\n");
    nrfx_gpiote_out_clear(LED1_PIN);
    etimer_set(&timer, 2 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    printf("Turning OFF LED1\n");
    nrfx_gpiote_out_set(LED1_PIN);
    etimer_set(&timer, 1 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    printf("Turning ON LED2 (P0.14)\n");
    nrfx_gpiote_out_clear(LED2_PIN);
    etimer_set(&timer, 2 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    printf("Turning OFF LED2\n");
    nrfx_gpiote_out_set(LED2_PIN);
    etimer_set(&timer, 1 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    printf("Turning ON LED3 (P0.15)\n");
    nrfx_gpiote_out_clear(LED3_PIN);
    etimer_set(&timer, 2 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    printf("Turning OFF LED3\n");
    nrfx_gpiote_out_set(LED3_PIN);
    etimer_set(&timer, 1 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    printf("Turning ON LED4 (P0.16)\n");
    nrfx_gpiote_out_clear(LED4_PIN);
    etimer_set(&timer, 2 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    printf("Turning OFF LED4\n");
    nrfx_gpiote_out_set(LED4_PIN);
    etimer_set(&timer, 1 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    printf("\n-- Cycle complete. Repeating...\n\n");
  }

  PROCESS_END();
}

