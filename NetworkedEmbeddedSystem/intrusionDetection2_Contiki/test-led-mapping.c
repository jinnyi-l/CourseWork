#include <stdio.h>
#include "contiki.h"
#include "dev/leds.h"
#include "sys/etimer.h"

/*
 * 📌 LED 测试：
 *   1️⃣ 依次点亮 LEDS_RED, LEDS_GREEN, LEDS_BLUE, LEDS_YELLOW，每个 LED 亮 2 秒后关闭。
 *   2️⃣ 观察 **物理板子** 上的 LED (LED1, LED2, LED3, LED4) 是否与预期匹配。
 *   3️⃣ 额外测试：一次性点亮所有 LED，确保它们是独立的。
 */

PROCESS(test_led_mapping, "Test LED Mapping");
AUTOSTART_PROCESSES(&test_led_mapping);

/* 🛠️ 额外函数：打印 GPIO 状态 (使用 GPIO 端口号) */
void print_gpio_state() {
#ifdef NRF_GPIO
  printf("GPIO state: LED1(P0.13)=%d, LED2(P0.14)=%d, LED3(P0.15)=%d, LED4(P0.16)=%d\n",
         (NRF_GPIO->OUT & (1 << 13)) ? 1 : 0,
         (NRF_GPIO->OUT & (1 << 14)) ? 1 : 0,
         (NRF_GPIO->OUT & (1 << 15)) ? 1 : 0,
         (NRF_GPIO->OUT & (1 << 16)) ? 1 : 0);
#else
  printf("NRF_GPIO not defined, skipping GPIO state check.\n");
#endif
}

PROCESS_THREAD(test_led_mapping, ev, data) {
  static struct etimer timer;
  PROCESS_BEGIN();

  printf("=== LED Mapping Test Started ===\n");
  printf("This program will sequentially turn on/off RED, GREEN, BLUE, YELLOW every 2s.\n");
  printf("Observe which physical LED on your board actually lights up!\n\n");

  while(1) {
    /* 🔴 1) RED on/off */
    printf("[Test] Turning ON LEDS_RED\n");
    leds_on(LEDS_RED);
    print_gpio_state();
    etimer_set(&timer, 2 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    printf("[Test] Turning OFF LEDS_RED\n");
    leds_off(LEDS_RED);
    print_gpio_state();
    etimer_set(&timer, 2 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    /* 🟢 2) GREEN on/off */
    printf("[Test] Turning ON LEDS_GREEN\n");
    leds_on(LEDS_GREEN);
    print_gpio_state();
    etimer_set(&timer, 2 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    printf("[Test] Turning OFF LEDS_GREEN\n");
    leds_off(LEDS_GREEN);
    print_gpio_state();
    etimer_set(&timer, 2 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    /* 🔵 3) BLUE on/off */
    printf("[Test] Turning ON LEDS_BLUE\n");
    leds_on(LEDS_BLUE);
    print_gpio_state();
    etimer_set(&timer, 2 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    printf("[Test] Turning OFF LEDS_BLUE\n");
    leds_off(LEDS_BLUE);
    print_gpio_state();
    etimer_set(&timer, 2 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    /* 🟡 4) YELLOW on/off */
    printf("[Test] Turning ON LEDS_YELLOW\n");
    leds_on(LEDS_YELLOW);
    print_gpio_state();
    etimer_set(&timer, 2 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    printf("[Test] Turning OFF LEDS_YELLOW\n");
    leds_off(LEDS_YELLOW);
    print_gpio_state();
    etimer_set(&timer, 2 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    /* 🔎 额外测试：同时点亮所有 LED */
    printf("[Test] Turning ON ALL LEDs\n");
    leds_on(LEDS_RED | LEDS_GREEN | LEDS_BLUE | LEDS_YELLOW);
    print_gpio_state();
    etimer_set(&timer, 3 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    printf("[Test] Turning OFF ALL LEDs\n");
    leds_off(LEDS_RED | LEDS_GREEN | LEDS_BLUE | LEDS_YELLOW);
    print_gpio_state();
    etimer_set(&timer, 3 * CLOCK_SECOND);
    PROCESS_WAIT_EVENT_UNTIL(etimer_expired(&timer));

    printf("\n-- Cycle complete. Repeating...\n\n");
  }

  PROCESS_END();
}

