program MotorController;

uses
  Forms,
  ControlPanel in 'ControlPanel.pas' {Form1},
  ChooseJoystick in 'ChooseJoystick.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'USB Rumble Motor Controller';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
