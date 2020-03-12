unit uEditFloat;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFEditFloat }

  TFEditFloat = class(TForm)
    BCancel: TButton;
    BOk: TButton;
    BReset: TButton;
    Edit: TEdit;
    lblExp: TLabel;
    lblValue: TLabel;
    procedure BResetClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
  private

  public
    FInitValue: Single;
  end;

var
  FEditFloat: TFEditFloat;

implementation

uses
  uCommon;

{$R *.lfm}

{ TFEditFloat }

procedure TFEditFloat.BResetClick(Sender: TObject);
var
  s: string;
begin
  Edit.Text := FloatToStr(FInitValue,FloatDotFormat);
end;

procedure TFEditFloat.EditChange(Sender: TObject);
var
  f: single;
begin
  f := StrToFloatDef(Edit.Text,0,FloatDotFormat);
  lblValue.Caption := Format('Value: %s',[FloatToStr(f,FloatDotFormat)]);
  lblExp.Caption := Format('Scientific: %e',[f]);
end;

end.

