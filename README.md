# MotorController

This is an application I wrote to control the rumble motors in Dualshock-like USB joysticks.  It is written in Delphi for Windows and I reused this code to control the motors in my [Lollybot](https://github.com/Tominator2/suckerbot) (a.k.a. Suckerbot) robot.

![motorcontroller](https://cloud.githubusercontent.com/assets/4344677/8122994/96022766-10ec-11e5-949e-4958b89262ee.png)

The code to control the motors can be found in [ControlPanel.pas](https://github.com/Tominator2/MotorController/blob/master/ControlPanel.pas).  I also added an option to choose the joystick from a list of available USB devices which is implemented in the file [ChooseJoystick.pas](https://github.com/Tominator2/MotorController/blob/master/ChooseJoystick.pas)

![choosejoystick](https://cloud.githubusercontent.com/assets/4344677/8122995/97444d7a-10ec-11e5-9c4e-6c4acb44cd47.png)


## Reverse Engineering the USB Commands

I reverse engineered the commands to control the motors using the USB sniffing tool   [SnoopyPro](http://sourceforge.net/projects/usbsnoop/files/SnoopyPro/).  I plugged a joystick into the computer, started monitoring the USB data with SnoopyPro, and then exercised the rumble motors using the standard Windows game controller tool.

When I examined the data being sent over USB it appeared that the single command `f3 00 00 00 00 00 00` was being sent (in the "down" direction) to stop the motor and that two commands were sent to start the motor: `51 00 fe 00 fe 00 00` followed by `fa fe 00 00 00 00 00`.  See a screenshot of the output below.

![snoopypro](https://cloud.githubusercontent.com/assets/4344677/8123091/c2edaea2-10ed-11e5-8ef4-c681b8753a72.png)

I observed that with different rumble "strengths" the two `fe` values would change.  These are two 8-bit values corresponding to the power level for each motor from `00` (off) to `fe` (full power).  See [ControlPanel.pas](https://github.com/Tominator2/MotorController/blob/master/ControlPanel.pas) for more details.


## Download

Note that the code uses the [HidController package](http://www.soft-gems.net/index.php/controls/human-interface-device-controller-suite) written by Robert Marquardt which you will need to install if you want to compile the Delphi project from the source code.

Download the executable for Windows: [MotorController.exe](https://github.com/Tominator2/MotorController/releases/download/v1.0/MotorController.exe) (458 KB)
