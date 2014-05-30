program Project1;

uses
  XPMan,
  Forms,
  DemoForm in 'DemoForm.pas' {FormDemo},
  ATScroll in 'ATScroll.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormDemo, FormDemo);
  Application.Run;
end.
