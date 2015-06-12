unit ChooseJoystick;

{/
   This form shows a list of connected USB devices so the user can select the
   joystick.

   To Do:
   ------
   Should edit the device list to only show those that have a Product Name
   containing either "game" or "joy".
/}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, JvHidControllerClass;

type
  TForm2 = class(TForm)
    Label1: TLabel;
    ListBox1: TListBox;
    ButtonOK: TButton;
    ButtonCancel: TButton;
    HidCtl: TJvHidDeviceController;
    procedure ButtonOKClick(Sender: TObject);
    procedure ButtonCancelClick(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    function HidCtlEnumerate(HidDev: TJvHidDevice;
      const Idx: Integer): Boolean;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    function DeviceName(HidDev: TJvHidDevice): string;
  public
    { Public declarations }
    function Show(params: TStrings): Integer;
  end;

var
  Form2: TForm2;
  localParams: TStrings; // used to pass back the PID and VID of the device

implementation

{$R *.dfm}

procedure TForm2.ButtonOKClick(Sender: TObject);
begin
  ModalResult := mrOK;
end;


procedure TForm2.ButtonCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;


function TForm2.Show(params: TStrings): Integer;
begin
  localParams := params;
  Result := ShowModal;  //call original show function
end;


procedure TForm2.ListBox1Click(Sender: TObject);
var
  Dev: TJvHidDevice;
begin
  // if valid joystick selected then set pid and vid
  Dev := TJvHidDevice(ListBox1.Items.Objects[ListBox1.ItemIndex]);
  localParams.Insert(0, IntToStr(Dev.Attributes.VendorID));
  localParams.Insert(1, IntToStr(Dev.Attributes.ProductID));
  ButtonOK.Enabled := True;
end;


function TForm2.HidCtlEnumerate(HidDev: TJvHidDevice;
  const Idx: Integer): Boolean;
var
  N: Integer;
  Dev: TJvHidDevice;
begin
  N := ListBox1.Items.Add(DeviceName(HidDev));
  HidCtl.CheckOutByIndex(Dev, Idx);
  Dev.NumInputBuffers := 128;
  Dev.NumOverlappedBuffers := 128;
  ListBox1.Items.Objects[N] := Dev;
  Result := True;
end;


function TForm2.DeviceName(HidDev: TJvHidDevice): string;
begin

  // should only add if in Game Controller group
  if HidDev.ProductName <> '' then
    Result := HidDev.ProductName
  else
    Result := Format('Device VID=%.4x PID=%.4x',
      [HidDev.Attributes.VendorID, HidDev.Attributes.ProductID]);
  if HidDev.SerialNumber <> '' then
    Result := Result + Format(' (Serial=%s)', [HidDev.SerialNumber]);
end;


procedure TForm2.FormShow(Sender: TObject);
begin
  if (ListBox1.Items.Count = 0) then
    HidCtl.Enumerate;
end;

end.
