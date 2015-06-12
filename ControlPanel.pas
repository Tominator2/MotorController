unit ControlPanel;

{/

  This is a USB joystick rumble motor controller reverse engineered using
  data obtained via the USB sniffing tool SnoopyPro. For more information
  see:

    https://github.com/Tominator2/MotorController

  Note that it uses the HidController package written by Robert Marquardt
  which you can download here:

    http://www.soft-gems.net/index.php/controls/human-interface-device-controller-suite

  This must be installed before you can compile the project.


  Notes/Ideas:
  ------------

  Can we read the motor state via USB or only control it?

  Sending the two 8 byte sequences:
    $00 51 00 XX 00 YY 00 00
    $00 fa fe 00 00 00 00 00
  causes the motors to rumble where:
    XX is the right motor "power" value from $00 to $fe (0 to 254)
    YY is the left  motor "power" value from $00 to $fe (0 to 254)

  Sending the 8 byte sequence:
    $00 f3 00 00 00 00 00 00

  Tested with a DiLong "Dualshock" USB and Oker joytsicks

  Note that Oker Twin USB joysticks (2 joysticks with a shared USB connection)
  give anerror.

  To Do:
  ------
  - Allow another joystick to be chosen
  - Only populate the list with game controllers/devices that support output
    (see WriteInstr)

/}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, JvHidControllerClass, StdCtrls, ComCtrls, ExtCtrls;

type
  MotorInstr = array[0..7] of Byte;

type
  TForm1 = class(TForm)
    ExitButton: TButton;
    SelectButton: TButton;
    LeftButton: TButton;
    RightButton: TButton;
    LeftPowerBar: TScrollBar;
    RightPowerBar: TScrollBar;
    LeftPowerLabel: TLabel;
    RightPowerLabel: TLabel;
    LeftGroupBox: TGroupBox;
    RightGroupBox: TGroupBox;
    HidCtl: TJvHidDeviceController;
    procedure ExitButtonClick(Sender: TObject);
    procedure LeftPowerBarChange(Sender: TObject);
    procedure RightPowerBarChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LeftButtonClick(Sender: TObject);
    procedure RightButtonClick(Sender: TObject);
    procedure SelectButtonClick(Sender: TObject);
  private
    { Private declarations }
    leftMotorOn, rightMotorOn: Boolean;
    usbVid, usbPid: Integer;
    usbDev: TJvHidDevice;
    procedure EnableButtons(isEnabled: Boolean);
  public
    { Public declarations }
    usbParams: TStrings;
    txInstr,  powerInstr, offInstr: MotorInstr;
    function  InstrToStr(instr: MotorInstr): String;
    procedure WriteInstr(instr: MotorInstr);
    procedure TxMotorInstruction();
  end;

var
  Form1: TForm1;

implementation

uses ChooseJoystick;

{$R *.dfm}


procedure TForm1.ExitButtonClick(Sender: TObject);
begin
  WriteInstr(offInstr); // send stop instruction
  Application.Terminate;
end;


procedure TForm1.LeftPowerBarChange(Sender: TObject);
begin
  LeftPowerLabel.Caption := IntToStr(LeftPowerBar.Position);
  if (leftMotorOn) then
    TxMotorInstruction();
end;


procedure TForm1.RightPowerBarChange(Sender: TObject);
begin
  RightPowerLabel.Caption := IntToStr(RightPowerBar.Position);
  if (rightMotorOn) then
    TxMotorInstruction();
end;


procedure TForm1.EnableButtons(isEnabled: Boolean);
begin
  LeftButton.Enabled  := isEnabled;
  RightButton.Enabled := isEnabled;
end;


procedure TForm1.FormCreate(Sender: TObject);
var
  I: Integer;
begin
  leftMotorOn  := False;
  rightMotorOn := False;
  LeftPowerBar.Position  := 254; // starting position
  RightPowerBar.Position := 128; // starting position

  // create instruction arrays

  // $00 51 00 00 00 00 00 00 - motor power levels
  powerInstr[0] := $00;
  powerInstr[1] := $51;
  for I := 2 to 7 do
    powerInstr[I] := $00;

  // $00 fa fe 00 00 00 00 00 - TX levels
  txInstr[0] := $00;
  txInstr[1] := $fa;
  txInstr[2] := $fe;
  for I := 3 to 7 do
    txInstr[I] := $00;

  // $00 f3 00 00 00 00 00 00 - stop
  offInstr[0] := $00;
  offInstr[1] := $f3;
  for I := 2 to 7 do
    offInstr[I] := $00;

end;


procedure TForm1.LeftButtonClick(Sender: TObject);
begin
  if (leftMotorOn) then
    begin
      leftMotorOn := False;
      LeftButton.Caption := 'Off';
    end
  else
    begin
       leftMotorOn := True;
       LeftButton.Caption := 'On';
    end;
   TxMotorInstruction();
end;


procedure TForm1.RightButtonClick(Sender: TObject);
begin
  if (rightMotorOn) then
    begin
      rightMotorOn := False;
      RightButton.Caption := 'Off';
    end
  else
    begin
       rightMotorOn := True;
       RightButton.Caption := 'On';
       //ShowMessage('Instr = ' + InstrToStr(GetMotorInstruction())); // trace
    end;
    TxMotorInstruction();
end;


// Transmit the Motor Instructions to the Joystick
procedure TForm1.TxMotorInstruction();
begin
   if (leftMotorOn) then
    powerInstr[5] := LeftPowerBar.Position  // set power level from slider
  else
    powerInstr[5] := $00;

  if (RightMotorOn) then
    powerInstr[3] := RightPowerBar.Position  // set power level from slider
  else
    powerInstr[3] := $00;

  // write to USB
  if (leftMotorOn or rightMotorOn) then
    begin
      WriteInstr(powerInstr); // set power levels
      WriteInstr(txInstr);
    end
  else
    writeInstr(offInstr);

end;


procedure TForm1.SelectButtonClick(Sender: TObject);
begin

  // USB values for PID and VID are passed back via
  // this string list  
  usbParams := TStringList.Create;

  if Form2.Show(usbParams) = mrOK then
    begin
      EnableButtons(True);
      usbVid := StrToIntDef(usbParams.Strings[0],0);
      usbPid := StrToIntDef(usbParams.Strings[1],0);
      HidCtl.CheckOutByID(usbDev,usbVid,usbPid);
      // disable the "Joystick Select" button to prevent double loading
      SelectButton.Enabled := False;

      //ShowMessage('Report byte length = ' + IntToStr(usbDev.Caps.OutputReportByteLength));
      WriteInstr(offInstr); // TX off command
    end
  else
    begin
      EnableButtons(False);
      usbVid := -1;
      usbPid := -1;
      usbDev := nil;
    end;

  //ShowMessage(IntToStr(usbVid) + ',' + IntToStr(usbPid)); // trace
  usbParams.Destroy;

end;


function TForm1.InstrToStr(instr: MotorInstr): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to 7 do
    begin
      Result := Result + ' ' + Format('%.2x', [instr[I]]);
    end;
end;


procedure TForm1.WriteInstr(instr: MotorInstr);
var
  Written: Cardinal;
  ToWrite: Cardinal;
  Err: DWORD;
begin
    // check that we have a USB device available
    if usbDev = nil then
      exit;

    // should this be tested elsewhere?
    ToWrite := usbDev.Caps.OutputReportByteLength;
    if not usbDev.WriteFile(instr, ToWrite, Written) then
    begin
      Err := GetLastError;
      ShowMessage(Format('WRITE ERROR: %s (%x)', [SysErrorMessage(Err), Err]));
    end
end;


end.
