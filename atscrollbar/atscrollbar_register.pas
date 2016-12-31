unit ATScrollbar_Register;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ATScrollbar, LResources;

procedure Register;

implementation


{ Registration }
procedure Register;
begin
  RegisterComponents('Misc', [TATScroll]);
end;

initialization
  {$I res/icons.lrs}

end.

