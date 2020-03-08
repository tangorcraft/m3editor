unit uEditInteger;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFEditInteger }

  TFEditInteger = class(TForm)
    BCancel: TButton;
    BOk: TButton;
    BReset: TButton;
    BFlags: TButton;
    EditSigned: TEdit;
    EditUnSigned: TEdit;
    EditHex: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure BResetClick(Sender: TObject);
    procedure EditHexChange(Sender: TObject);
    procedure EditSignedChange(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure EditUnSignedChange(Sender: TObject);
  private
    procedure UpdateEdits(const Exclude: TEdit);
  public
    FCurValue: UInt32;
    FInitValue: UInt32;
  end;

var
  FEditInteger: TFEditInteger;

implementation

{$R *.lfm}

{ TFEditInteger }

procedure TFEditInteger.BResetClick(Sender: TObject);
begin
  FCurValue := FInitValue;
  UpdateEdits(nil);
end;

procedure TFEditInteger.EditHexChange(Sender: TObject);
begin
  FCurValue := StrToIntDef('0x'+EditHex.Text,0);
  UpdateEdits(EditHex);
end;

procedure TFEditInteger.EditSignedChange(Sender: TObject);
var
  i: Int32;
begin
  i := StrToIntDef(EditSigned.Text,0);
  FCurValue := i;
  UpdateEdits(EditSigned);
end;

procedure TFEditInteger.EditExit(Sender: TObject);
begin
  UpdateEdits(nil);
end;

procedure TFEditInteger.EditUnSignedChange(Sender: TObject);
begin
  FCurValue := StrToIntDef(EditUnSigned.Text,0);
  UpdateEdits(EditUnSigned);
end;

procedure TFEditInteger.UpdateEdits(const Exclude: TEdit);
begin
  if EditSigned<>Exclude then
    EditSigned.Text := IntToStr(Int32(FCurValue));
  if EditUnSigned<>Exclude then
    EditUnSigned.Text := IntToStr(FCurValue);
  if EditHex<>Exclude then
    EditHex.Text := IntToHex(FCurValue and $FFFF,8);
end;

end.

