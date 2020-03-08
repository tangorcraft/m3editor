unit uEditWord;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFEditWord }

  TFEditWord = class(TForm)
    BCancel: TButton;
    BFlags: TButton;
    BOk: TButton;
    BReset: TButton;
    EditHex: TEdit;
    EditSigned: TEdit;
    EditUnSigned: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblFixed: TLabel;
    procedure BResetClick(Sender: TObject);
    procedure EditHexChange(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure EditSignedChange(Sender: TObject);
    procedure EditUnSignedChange(Sender: TObject);
  private
    procedure UpdateEdits(const Exclude: TEdit);
  public
    FCurValue: UInt16;
    FInitValue: UInt16;
  end;

var
  FEditWord: TFEditWord;

implementation

{$R *.lfm}

{ TFEditWord }

procedure TFEditWord.BResetClick(Sender: TObject);
begin
  FCurValue := FInitValue;
  UpdateEdits(nil);
end;

procedure TFEditWord.EditHexChange(Sender: TObject);
begin
  FCurValue := StrToIntDef('0x'+EditHex.Text,0);
  UpdateEdits(EditHex);
end;

procedure TFEditWord.EditExit(Sender: TObject);
begin
  UpdateEdits(nil);
end;

procedure TFEditWord.EditSignedChange(Sender: TObject);
var
  i: Int16;
begin
  i := StrToIntDef(EditSigned.Text,0);
  FCurValue := i;
  UpdateEdits(EditSigned);
end;

procedure TFEditWord.EditUnSignedChange(Sender: TObject);
begin
  FCurValue := StrToIntDef(EditUnSigned.Text,0);
  UpdateEdits(EditUnSigned);
end;

procedure TFEditWord.UpdateEdits(const Exclude: TEdit);
begin
  if EditSigned<>Exclude then
    EditSigned.Text := IntToStr(Int16(FCurValue));
  if EditUnSigned<>Exclude then
    EditUnSigned.Text := IntToStr(FCurValue);
  if EditHex<>Exclude then
    EditHex.Text := IntToHex(FCurValue,4);
  lblFixed.Caption := Format('Fixed: %f',[FCurValue / 2048.0]);
end;

end.

