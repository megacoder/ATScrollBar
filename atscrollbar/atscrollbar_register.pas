unit ATScrollbar_Register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ATScrollbar;

procedure Register;

implementation

//{$R atscrollbar.dcr}

{ Registration }
procedure Register;
begin
  RegisterComponents('Misc', [TATScroll]);
end;

end.

