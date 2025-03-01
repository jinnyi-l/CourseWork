
configuration MovementDetectAppC { }
implementation {
  components MainC, MovementDetectC as App;
  components LedsC;
  components new TimerMilliC() as TimerAccel;

  components new ADXL345C() as Accel;

  App.Boot -> MainC.Boot;
  App.TimerAccel -> TimerAccel.Timer;
  App.Leds -> LedsC;

  App.AccelControl -> Accel.SplitControl;
  App.AccelX -> Accel.X;
  App.AccelY -> Accel.Y;
  App.AccelZ -> Accel.Z;
}
