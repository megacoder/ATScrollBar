{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit atscrollbar_pkg;

{$warn 5023 off : no warning about unused units}
interface

uses
  ATScrollBar, ATScrollbar_Register, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('ATScrollbar_Register', @ATScrollbar_Register.Register);
end;

initialization
  RegisterPackage('atscrollbar_pkg', @Register);
end.
