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
    procedure BResetClick(Sender: TObject);
  private

  public
    FInitValue: Single;
  end;

var
  FEditFloat: TFEditFloat;

implementation

{$R *.lfm}

{ TFEditFloat }

procedure TFEditFloat.BResetClick(Sender: TObject);
begin
  Edit.Text := FloatToStrF(FInitValue,ffFixed,15,0);
end;

end.

