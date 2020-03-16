(*
    This file is a part of "M3 Editor" project <https://github.com/tangorcraft/m3editor/>.
    Copyright (C) 2020  Ivan Markov (TangorCraft)

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
*)
unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  ExtCtrls, ustructures, uM3File, uTagEditor, IniFiles, uCHARBulkEdit,
  uM3ML;

type

  { TFMain }

  TFMain = class(TForm)
    btnTreeViewEditor: TButton;
    BMeshEditor: TButton;
    btnBulkEditCHAR: TButton;
    cbAskOnJumpTo: TCheckBox;
    cbAskOnCharAutoUpdate: TCheckBox;
    cbRememberStructFile: TCheckBox;
    gbOptions: TGroupBox;
    lblLastFile: TLabel;
    lblStruct: TLabel;
    MainMenu: TMainMenu;
    MemoLog: TMemo;
    MDebugAction: TMenuItem;
    MDebug1: TMenuItem;
    MScanRefCHAR: TMenuItem;
    MScanRefAll: TMenuItem;
    MScanRef: TMenuItem;
    MOpenM3ML: TMenuItem;
    N3: TMenuItem;
    MSaveM3ML: TMenuItem;
    N2: TMenuItem;
    MExport: TMenuItem;
    MModel: TMenuItem;
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
    SaveM3MLDialog: TSaveDialog;
    procedure btnBulkEditCHARClick(Sender: TObject);
    procedure btnTreeViewEditorClick(Sender: TObject);
    procedure cbAskOnCharAutoUpdateChange(Sender: TObject);
    procedure cbAskOnJumpToChange(Sender: TObject);
    procedure cbRememberStructFileChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MDebugActionClick(Sender: TObject);
    procedure MFileOpenClick(Sender: TObject);
    procedure MSaveAsClick(Sender: TObject);
    procedure MSaveClick(Sender: TObject);
    procedure MSaveM3MLClick(Sender: TObject);
    procedure MScanRefAllClick(Sender: TObject);
    procedure MScanRefCHARClick(Sender: TObject);
    procedure MStructOpenClick(Sender: TObject);
    procedure MStructReloadClick(Sender: TObject);
  private
    FAppPath: string;

    FStructFileName: string;
    FCurrentFileName: string;
    FLastSavedFileName: string;

    FM3File: TM3File;
    FModified: Boolean;
    FTagEditor: TFTagEditor;

    procedure UpdateLabels;
    procedure StructuresUpdate;
  public
    procedure Log(const S: string);
    procedure Log(const Fmt : string; const Args : Array of const);

    procedure FreeTagEditor;
    procedure ModelChanged(const Changer: TForm);
  end;

var
  FMain: TFMain;
  IniMain: TIniFile;

implementation

uses
  uCommon;

{$R *.lfm}

{ TFMain }

procedure TFMain.FormCreate(Sender: TObject);
begin
  FAppPath := ExtractFilePath(Application.ExeName);
  IniMain := TIniFile.Create(FAppPath+'settings.ini');
  cbAskOnJumpTo.Checked := IniMain.ReadBool('treeView','askOnJump',true);
  cbAskOnCharAutoUpdate.Checked := IniMain.ReadBool('treeView','askCHARAuto',true);
  FStructFileName := IniMain.ReadString('main','struct',FAppPath+'structures.xml');
  cbRememberStructFile.Checked := IniMain.ReadBool('main','rememberStruct',false);
  Structures := TM3Structures.Create;
  FM3File := TM3File.Create;
  FModified := False;
  if cbRememberStructFile.Checked then
  begin
    if not FileExists(FStructFileName) then
    begin
      Log('Can''t find "%s"',[FStructFileName]);
      FStructFileName := FAppPath+'structures.xml';
      IniMain.WriteString('main','struct',FStructFileName);
    end;
  end
  else
    FStructFileName := FAppPath+'structures.xml';
  if FileExists(FStructFileName) then
  begin
    Log('Loading structures info from "%s"',[FStructFileName]);
    Structures.LoadStructures(FStructFileName);
    UpdateLabels;
  end
  else
  begin
    Log('Can''t find "%s"',[FStructFileName]);
    FStructFileName := '';
  end;
end;

procedure TFMain.btnTreeViewEditorClick(Sender: TObject);
begin
  if FTagEditor = nil then
  begin
    Application.CreateForm(TFTagEditor,FTagEditor);
    FTagEditor.ShowEditor(FM3File);
    btnTreeViewEditor.Enabled := false;
  end
  else
  begin
    FTagEditor.Free;
    FTagEditor := nil;
  end;
end;

procedure TFMain.btnBulkEditCHARClick(Sender: TObject);
begin
  with TFCHARBulkEdit.Create(Self) do
  try
    ShowEditor(FM3File);
  finally
    Free;
  end;
end;

procedure TFMain.cbRememberStructFileChange(Sender: TObject);
begin
  IniMain.WriteBool('main','rememberStruct',cbRememberStructFile.Checked);
  if cbRememberStructFile.Checked then
    IniMain.WriteString('main','struct',FStructFileName);
end;

procedure TFMain.cbAskOnCharAutoUpdateChange(Sender: TObject);
begin
  IniMain.WriteBool('treeView','askCHARAuto',cbAskOnCharAutoUpdate.Checked);
end;

procedure TFMain.cbAskOnJumpToChange(Sender: TObject);
begin
  IniMain.WriteBool('treeView','askOnJump',cbAskOnJumpTo.Checked);
end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if
    FModified and
    (MessageDlg(
      'Model changed',
      'All unsaved changes made to the model will be lost if you continue.',
      mtWarning,
      mbOKCancel,0
    ) <> mrOK)
  then CloseAction := caNone;
end;

procedure TFMain.FormDestroy(Sender: TObject);
begin
  FTagEditor.Free;
  Structures.Free;
  FM3File.Free;
  IniMain.Free;
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  {$IFOPT D-}
  BMeshEditor.Visible := false;
  MDebugAction.Visible := false;
  {$ENDIF}
end;

procedure TFMain.MDebugActionClick(Sender: TObject);
begin

end;

procedure TFMain.MFileOpenClick(Sender: TObject);
begin
  if
    FModified and
    (MessageDlg(
      'Model changed',
      'All unsaved changes made to the model will be lost if you continue.',
      mtWarning,
      mbOKCancel,0
    ) <> mrOK)
  then Exit;
  if OpenDialog.Execute then
  begin
    Log('Opening "%s"',[OpenDialog.FileName]);
    if IsValidM3File(OpenDialog.FileName) then
    begin
      FCurrentFileName := OpenDialog.FileName;
      FM3File.LoadM3File(OpenDialog.FileName);
      FModified := false;
      Log('%d tags loaded from "%s"',[FM3File.TagCount, OpenDialog.FileName]);
    end
    else
    begin
      Log('Parsing M3ML file: "%s"',[OpenDialog.FileName]);
      ImportFromM3ML(FM3File, OpenDialog.FileName);
      FModified := true;
      FCurrentFileName := '';
    end;
    StructuresUpdate;
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
    FModified := false;
    UpdateLabels;
  end;
end;

procedure TFMain.MSaveClick(Sender: TObject);
begin
  if (FCurrentFileName = '') or (not DirectoryExists(ExtractFileDir(FCurrentFileName))) then
  begin
    MSaveAsClick(nil);
    Exit;
  end;
  if FileExists(FCurrentFileName) and (FCurrentFileName <> FLastSavedFileName) then
  begin
    if MessageDlg('Save file','Replace file "'+FCurrentFileName+'"?',mtConfirmation,mbOKCancel,0)<>mrOK then Exit;
  end;
  Log('Saving to "%s"',[FCurrentFileName]);
  FM3File.SaveM3File(FCurrentFileName);
  FModified := false;
  FLastSavedFileName := FCurrentFileName;
end;

procedure TFMain.MSaveM3MLClick(Sender: TObject);
begin
  if SaveM3MLDialog.Execute then
  begin
    Log('Exporting model data to "%s"',[SaveM3MLDialog.FileName]);
    ExportToM3ML(FM3File,SaveM3MLDialog.FileName);
  end;
end;

procedure TFMain.MScanRefAllClick(Sender: TObject);
begin
  Log('Scanning for possible reference count mismatch or invalid index.');
  Log('Reference count mismatch found and repaired: %d',[FM3File.ScanReferences(false)]);
end;

procedure TFMain.MScanRefCHARClick(Sender: TObject);
begin
  Log('Scanning for possible reference count mismatch (ref to CHAR only) or invalid index.');
  Log('Reference count mismatch found and repaired: %d',[FM3File.ScanReferences(true)]);
end;

procedure TFMain.MStructOpenClick(Sender: TObject);
begin
  if OpenStructDialog.Execute then
  begin
    FStructFileName := OpenStructDialog.FileName;
    Log('Loading structures info from "%s"',[FStructFileName]);
    Structures.LoadStructures(FStructFileName);
    IniMain.WriteString('main','struct',FStructFileName);
    StructuresUpdate;
  end;
end;

procedure TFMain.MStructReloadClick(Sender: TObject);
begin
  if FileExists(FStructFileName) then
  begin
    Log('Reloading structures info from "%s"',[FStructFileName]);
    Structures.LoadStructures(FStructFileName);
    StructuresUpdate;
  end;
end;

procedure TFMain.UpdateLabels;
begin
  lblStruct.Caption := Format('Structures File: "%s"',[FStructFileName]);
  lblLastFile.Caption := Format('Last Opened File: "%s"',[FCurrentFileName]);
end;

procedure TFMain.StructuresUpdate;
begin

  if FTagEditor <> nil then
    FTagEditor.ResetTagTree;
  FM3File.ResetRefFrom;
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

procedure TFMain.FreeTagEditor;
begin
  if FTagEditor <> nil then
    FTagEditor := nil;
  btnTreeViewEditor.Enabled := true;
end;

procedure TFMain.ModelChanged(const Changer: TForm);
begin
  FModified := true;
  FM3File.ResetRefFrom;
  if (FTagEditor <> nil) and (Changer <> FTagEditor) then
    FTagEditor.ResetTagTree;
end;

end.

