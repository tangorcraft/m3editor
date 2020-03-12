program m3editor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umain, ustructures, uTagEditor, uM3File, uEditString, uEditInteger,
  uEditFlags, uEditFloat, uEditWord, uEditByte, uCHARBulkEdit, uM3ML, uCommon,
  uRefEdit
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.MainFormOnTaskBar := True;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.

