#include "printfZ1.h"


module MovementDetectC {
  uses {
    interface Boot;
    interface Timer<TMilli> as TimerAccel;
    interface SplitControl as AccelControl;
    interface Read<uint16_t> as AccelX;
    interface Read<uint16_t> as AccelY;
    interface Read<uint16_t> as AccelZ;
    interface Leds;
  }
}



implementation {

  // Define the threshold for movement detection.
  int16_t const THRESHOLD = 50;

  // Define the time lights remains on
  int16_t const LIGHTON = 10;
  
  //first time sampling do not have old values, need to analyzing individually
  bool firstTime = TRUE;

  // Global variables
  int16_t x,y,z;
  int16_t oldX, oldY, oldZ;

  uint8_t xCounter = 0;
  uint8_t yCounter = 0;
  uint8_t zCounter = 0;



  //start event 
  event void Boot.booted(){
    printfz1_init();
    call AccelControl.start(); //start accelerometer
  }
  
  
  
  
  // Check if accelerometer started properly and start periodic data collection
  event void AccelControl.startDone(error_t result){
    if(result == SUCCESS){
       call TimerAccel.startPeriodic(100); //10Hz = 100ms
    }
  }
  
  event void AccelControl.stopDone(error_t result){}
  
  
  
  
  //read x-axis every 100ms, call the AccelX function every 100ms
  event void TimerAccel.fired(){
    call AccelX.read();
  }
  
  //calculate the data in three axises
  event void AccelX.readDone(error_t result, uint16_t val){
    if(result == SUCCESS){
      x = (int16_t)(val - 512);
    }
    
    call AccelY.read();
  }
  
  event void AccelY.readDone(error_t result, uint16_t val) {
    if (result == SUCCESS) {
        y = (int16_t)(val - 512);
            }
    call AccelZ.read();
  }

  event void AccelZ.readDone(error_t result, uint16_t val) {
    if (result == SUCCESS) {
        z = (int16_t)(val - 512); //reading process stop here; start analyzing
    }



    if(!firstTime){
  
      if(abs(x - oldX) > THRESHOLD){
        xCounter = LIGHTON; //turn on the light       
      }else if (xCounter > 0){
        xCounter --;
      //the motion no longer detected but the light on, start countdown
     }
     
      if(abs(y - oldY) > THRESHOLD){
        yCounter = LIGHTON;
      }else if(yCounter > 0){
        yCounter --;
      }
      
      if(abs(z - oldZ) > THRESHOLD){
        zCounter = LIGHTON;
      }else if(zCounter > 0){
        zCounter --;
      }
    
    }else{
      firstTime = FALSE; 
      //comparison between x and oldX begines in the second sampling; Directly update current value as oldXYZ
    }
    
    //Update xyz 
    oldX = x;
    oldY = y;
    oldZ = z;
    
    
    //light control 
    if(xCounter > 0){
      call Leds.led0On();
    } else {
      call Leds.led0Off();
    }
    
    if (yCounter > 0) {
      call Leds.led1On();
    } else {
      call Leds.led1Off();
    }

    if (zCounter > 0) {
      call Leds.led2On();
    } else {
      call Leds.led2Off();
    }
  }
}