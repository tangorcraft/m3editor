unit uColorEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ustructures, uM3File;

type

  { TFEditColor }

  TFEditColor = class(TForm)
    BCancel: TButton;
    BOk: TButton;
    BReset: TButton;
    ColorDialog: TColorDialog;
    ShapeOld: TShape;
    ShapeNew: TShape;
    procedure BResetClick(Sender: TObject);
  private

  public

  end;

var
  FEditColor: TFEditColor;

implementation

uses
  uCommon;

{$R *.lfm}

{ TFEditColor }

procedure TFEditColor.BResetClick(Sender: TObject);
begin
  ColorDialog.Execute;
end;

end.

