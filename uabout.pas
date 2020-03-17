unit uAbout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TFAbout }

  TFAbout = class(TForm)
    BOK: TButton;
    BSource: TButton;
    BLicense: TButton;
    BLazarus: TButton;
    ImageLaz: TImage;
    Label1: TLabel;
    procedure BLazarusClick(Sender: TObject);
    procedure BLicenseClick(Sender: TObject);
    procedure BSourceClick(Sender: TObject);
  private

  public

  end;

var
  FAbout: TFAbout;

implementation

uses
  LCLIntf;

{$R *.lfm}

{ TFAbout }

procedure TFAbout.BSourceClick(Sender: TObject);
begin
  OpenURL('https://github.com/tangorcraft/m3editor');
end;

procedure TFAbout.BLicenseClick(Sender: TObject);
begin
  OpenURL('https://www.gnu.org/licenses/');
end;

procedure TFAbout.BLazarusClick(Sender: TObject);
begin
  OpenURL('https://www.lazarus-ide.org/');
end;

end.

