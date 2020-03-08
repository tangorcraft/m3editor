unit uEditString;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFEditString }

  TFEditString = class(TForm)
    BOk: TButton;
    BCancel: TButton;
    BReset: TButton;
    Edit: TEdit;
    procedure BResetClick(Sender: TObject);
  private

  public
    FInitalVal: string;
  end;

var
  FEditString: TFEditString;

implementation

{$R *.lfm}

{ TFEditString }

procedure TFEditString.BResetClick(Sender: TObject);
begin
  Edit.Text := FInitalVal;
end;

end.

