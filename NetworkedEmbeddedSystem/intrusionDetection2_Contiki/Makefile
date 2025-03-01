# Simple makefile for Task 2
# 同时编译 "intrusion-basestation-task2" 和 "intrusion-client-task2"
CONTIKI_PROJECT = intrusion-basestation-task2 intrusion-client-task2

all: $(CONTIKI_PROJECT)

# 使用 NullNet + CSMA
MAKE_MAC = MAKE_MAC_CSMA
MAKE_NET = MAKE_NET_NULLNET

# 修改为你的 Contiki-NG 路径
CONTIKI = /home/wcnes/contiki-ng-wcnes

include $(CONTIKI)/Makefile.include

