unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  ExtCtrls, ustructures, uM3File, uTagEditor;

type

  { TFMain }

  TFMain = class(TForm)
    btnTreeViewEditor: TButton;
    lblLastFile: TLabel;
    lblStruct: TLabel;
    MainMenu: TMainMenu;
    MemoLog: TMemo;
    MStructOpen: TMenuItem;
    MStructReload: TMenuItem;
    MStruct: TMenuItem;
    MSaveAs: TMenuItem;
    MSave: TMenuItem;
    MExit: TMenuItem;
    N1: TMenuItem;
    MFileOpen: TMenuItem;
    MFile: TMenuItem;
    OpenDialog: TOpenDialog;
    OpenStructDialog: TOpenDialog;
    PanelMain: TPanel;
    SaveDialog: TSaveDialog;
    Splitter: TSplitter;
    procedure btnTreeViewEditorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MFileOpenClick(Sender: TObject);
    procedure MSaveAsClick(Sender: TObject);
    procedure MSaveClick(Sender: TObject);
  private
    FAppPath: string;

    FStructFileName: string;
    FCurrentFileName: string;
    FLastSavedFileName: string;

    FM3File: TM3File;
    FTagEditors: array[0..9] of TFTagEditor;

    procedure UpdateLabels;
  public
    procedure Log(const S: string);
    procedure Log(const Fmt : string; const Args : Array of const);
  end;

var
  FMain: TFMain;

implementation

{$R *.lfm}

{ TFMain }

procedure TFMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  FAppPath := ExtractFilePath(Application.ExeName);
  Structures := TM3Structures.Create;
  FM3File := TM3File.Create;
  if FileExists(FAppPath+'structures.xml') then
  begin
    FStructFileName := FAppPath+'structures.xml';
    Log('Loading structures info from "%s"',[FStructFileName]);
    Structures.LoadStructures(FStructFileName);
    UpdateLabels;
  end
  else
    Log('Can'' find "%s"',[FAppPath+'structures.xml']);
end;

procedure TFMain.btnTreeViewEditorClick(Sender: TObject);
begin
  with TFTagEditor.Create(Self) do
  try
    ShowEditor(FM3File,true);
  finally
    Free;
  end;
end;

procedure TFMain.FormDestroy(Sender: TObject);
//var
//  i: integer;
begin
  //for i := 0 to 9 do
  //  FTagEditors[i].Free;
  Structures.Free;
  FM3File.Free;
end;

procedure TFMain.MFileOpenClick(Sender: TObject);
var
  i: integer;
begin
  if OpenDialog.Execute then
  begin
    Log('Opening "%s"',[OpenDialog.FileName]);
    FCurrentFileName := OpenDialog.FileName;
    FM3File.LoadM3File(OpenDialog.FileName);
    Log('%d tags loaded from "%s"',[FM3File.TagCount, OpenDialog.FileName]);
    for i := 0 to FM3File.TagCount - 1 do
      Structures.GetStructureInfo(FM3File[i]^);
    UpdateLabels;
  end;
end;

procedure TFMain.MSaveAsClick(Sender: TObject);
begin
  if (FM3File.TagCount > 0) and SaveDialog.Execute then
  begin
    FCurrentFileName := SaveDialog.FileName;
    FLastSavedFileName := SaveDialog.FileName;
    Log('Saving to "%s"',[FCurrentFileName]);
    FM3File.SaveM3File(FCurrentFileName);
    UpdateLabels;
  end;
end;

procedure TFMain.MSaveClick(Sender: TObject);
begin
  if (FCurrentFileName = '') or (not DirectoryExists(ExtractFileDir(FCurrentFileName))) then Exit;
  if FileExists(FCurrentFileName) and (FCurrentFileName <> FLastSavedFileName) then
  begin
    if MessageDlg('Save file','Replace file "'+FCurrentFileName+'"?',mtConfirmation,mbOKCancel,0)<>mrOK then Exit;
  end;
  Log('Saving to "%s"',[FCurrentFileName]);
  FM3File.SaveM3File(FCurrentFileName);
  FLastSavedFileName := FCurrentFileName;
end;

procedure TFMain.UpdateLabels;
begin
  lblStruct.Caption := Format('Structures File: "%s"',[FStructFileName]);
  lblLastFile.Caption := Format('Last Opened File: "%s"',[FCurrentFileName]);
end;

procedure TFMain.Log(const S: string);
begin
  if MemoLog.Lines.Count >= 1000 then
    MemoLog.Lines.Delete(0);
  MemoLog.Lines.Add(FormatDateTime('[hh:mm:ss] ',Now)+S);
end;

procedure TFMain.Log(const Fmt: string; const Args: array of const);
begin
  if MemoLog.Lines.Count >= 1000 then
    MemoLog.Lines.Delete(0);
  MemoLog.Lines.Add(FormatDateTime('[hh:mm:ss] ',Now)+Fmt,Args);
end;

end.

