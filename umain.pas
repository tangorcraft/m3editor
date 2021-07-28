(*
    This file is a part of "M3 Editor" project <https://github.com/tangorcraft/m3editor/>.
    Copyright (C) 2020-2021  Ivan Markov (TangorCraft)

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
  uM3ML, UAnimListView, u3DViewForm, uToolTextureRename, uToolBatchScan;

type
  TM3LogEvent = procedure (const S: string) of object;

  { TFMain }

  TFMain = class(TForm)
    btnTreeViewEditor: TButton;
    BMeshEditor: TButton;
    btnBulkEditCHAR: TButton;
    BAminList: TButton;
    cbAskOnJumpTo: TCheckBox;
    cbTreeViewNewMode: TCheckBox;
    cbRememberStructFile: TCheckBox;
    gbOptions: TGroupBox;
    lblLastFile: TLabel;
    lblStruct: TLabel;
    MainMenu: TMainMenu;
    MemoLog: TMemo;
    MDebugAction: TMenuItem;
    MDebug1: TMenuItem;
    MAbout: TMenuItem;
    MToolFixBoneScale: TMenuItem;
    MTextureTool: TMenuItem;
    MToolBatchScan: TMenuItem;
    MTools: TMenuItem;
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
    procedure BAminListClick(Sender: TObject);
    procedure BMeshEditorClick(Sender: TObject);
    procedure btnBulkEditCHARClick(Sender: TObject);
    procedure btnTreeViewEditorClick(Sender: TObject);
    procedure cbTreeViewNewModeChange(Sender: TObject);
    procedure cbAskOnJumpToChange(Sender: TObject);
    procedure cbRememberStructFileChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MAboutClick(Sender: TObject);
    procedure MDebugActionClick(Sender: TObject);
    procedure MFileOpenClick(Sender: TObject);
    procedure MSaveAsClick(Sender: TObject);
    procedure MSaveClick(Sender: TObject);
    procedure MSaveM3MLClick(Sender: TObject);
    procedure MScanRefAllClick(Sender: TObject);
    procedure MScanRefCHARClick(Sender: TObject);
    procedure MStructOpenClick(Sender: TObject);
    procedure MStructReloadClick(Sender: TObject);
    procedure MTextureToolClick(Sender: TObject);
    procedure MToolFixBoneScaleClick(Sender: TObject);
  private
    FOnLog: TM3LogEvent;

    FStructFileName: string;
    FInternalsFileName: string;
    FCurrentFileName: string;
    FLastSavedFileName: string;

    FM3File: TM3File;
    FModified: Boolean;
    FTagEditor: TFTagEditor;
    FAnimListForm: TFAnimListView;
    F3DViewForm: TF3dView;

    procedure UpdateLabels;
    procedure StructuresUpdate;

    procedure TryOpenFile(const FileName: string);

    procedure InitGL(var rActive: Boolean);
    procedure FrameStartGL(var rActive: Boolean);
    procedure RenderGL(var rActive: Boolean);
  public
    procedure Log(const S: string);
    procedure Log(const Fmt : string; const Args : Array of const);

    procedure FreeTagEditor;
    procedure FreeAnimListForm;
    procedure Free3DViewForm;
    procedure ModelChanged(const Changer: TForm);

    property OnLog: TM3LogEvent read FOnLog write FOnLog;
  end;

var
  FMain: TFMain;
  IniMain: TIniFile;
  FAppPath: string;

implementation

uses
  uCommon, uAbout,
  {$IFDEF MSWINDOWS}
  RenderWnd,
  {$Else}
  RenderDummy,
  {$ENDIF}
  RenderUtils, dglOpenGL, UToolFixBoneScale;

{$R *.lfm}

{ TFMain }

procedure TFMain.FormCreate(Sender: TObject);
begin
  FAppPath := ExtractFilePath(Application.ExeName);
  IniMain := TIniFile.Create(FAppPath+'settings.ini');
  cbAskOnJumpTo.Checked := IniMain.ReadBool('treeView','askOnJump',true);
  cbTreeViewNewMode.Checked := IniMain.ReadBool('treeView','TreeViewMode',true);
  FStructFileName := IniMain.ReadString('main','struct',FAppPath+'structures.xml');
  cbRememberStructFile.Checked := IniMain.ReadBool('main','rememberStruct',false);
  Structures := TM3Structures.Create;
  FM3File := TM3File.Create;
  FModified := False;
  FInternalsFileName := FAppPath+'internals.xml';
  if FileExists(FInternalsFileName) then
  begin
    Log('Loading internal info from "%s"',[FInternalsFileName]);
    Structures.LoadInternals(FInternalsFileName);
  end
  else
  begin
    Log('WARNING! Internal info file not found "%s"',[FAppPath+'internals.xml']);
  end;
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
  TryOpenFile(ParamStr(1));
  if Structures.InternalsChanged then
  begin
    Log('Updating "%s"',[FInternalsFileName]);
    Structures.SaveInternals(FInternalsFileName);
  end;

  // Render Init
  if not InitOpenGL then
  begin
    Log('OpenGL initialization failed!');
    Exit;
  end;
  if not ImplementationRead then
    ReadImplementationProperties;
  if not ExtensionsRead then
    ReadExtensions;
  SetRenderEventProc(@InitGL,RENDER_EVENT_INIT_GL);
  SetRenderEventProc(@FrameStartGL,RENDER_EVENT_FRAME_START_GL);
  SetRenderEventProc(@RenderGL,RENDER_EVENT_BEGIN_GL);
  SetRenderWindowsListCount(1);
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

procedure TFMain.BMeshEditorClick(Sender: TObject);
begin
  if F3DViewForm = nil then
  begin
    WndRenderStart;
    Application.CreateForm(TF3dView,F3DViewForm);
    F3DViewForm.ShowEditor(FM3File);
  end
  else if F3DViewForm.Visible then
  begin
    F3DViewForm.BringToFront;
  end
  else
  begin
    F3DViewForm.Free;
    F3DViewForm := nil;
    Application.ProcessMessages;
    WndRenderStart;
    Application.CreateForm(TF3dView,F3DViewForm);
    F3DViewForm.ShowEditor(FM3File);
  end;
end;

procedure TFMain.BAminListClick(Sender: TObject);
begin
  if FAnimListForm = nil then
  begin
    Application.CreateForm(TFAnimListView,FAnimListForm);
    FAnimListForm.ShowEditor(FM3File);
    BAminList.Enabled := false;
  end
  else
  begin
    FAnimListForm.Free;
    FAnimListForm := nil;
  end;
end;

procedure TFMain.cbRememberStructFileChange(Sender: TObject);
begin
  IniMain.WriteBool('main','rememberStruct',cbRememberStructFile.Checked);
  if cbRememberStructFile.Checked then
    IniMain.WriteString('main','struct',FStructFileName);
end;

procedure TFMain.cbTreeViewNewModeChange(Sender: TObject);
begin
  IniMain.WriteBool('treeView','TreeViewMode',cbTreeViewNewMode.Checked);
  if Assigned(FTagEditor) then
    FTagEditor.ResetTagTree;
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
  MDebugAction.Visible := false;
  MTools.Visible := False;
  {$ENDIF}
end;

procedure TFMain.MAboutClick(Sender: TObject);
begin
  with TFAbout.Create(self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TFMain.MDebugActionClick(Sender: TObject);
var
  i: UInt32;
  b: UInt8;
begin
  i := $800;
  b := $80;
  move(b,i,1);
  ShowMessage(IntToHex(i,8));
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
    TryOpenFile(OpenDialog.FileName);
  if Structures.InternalsChanged then
  begin
    Log('Updating "%s"',[FInternalsFileName]);
    Structures.SaveInternals(FInternalsFileName);
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
    if Structures.InternalsChanged then
    begin
      Log('Updating "%s"',[FInternalsFileName]);
      Structures.SaveInternals(FInternalsFileName);
    end;
    StructuresUpdate;
  end;
end;

procedure TFMain.MStructReloadClick(Sender: TObject);
begin
  if FileExists(FStructFileName) then
  begin
    Log('Reloading structures info from "%s"',[FStructFileName]);
    Structures.LoadStructures(FStructFileName);
    if Structures.InternalsChanged then
    begin
      Log('Updating "%s"',[FInternalsFileName]);
      Structures.SaveInternals(FInternalsFileName);
    end;
    StructuresUpdate;
  end;
end;

procedure TFMain.MTextureToolClick(Sender: TObject);
begin
  with TFTextureRename.Create(Self) do
  try
    ShowEditor(FM3File);
  finally
    Free;
  end;
end;

procedure TFMain.MToolFixBoneScaleClick(Sender: TObject);
begin
  FixNegativeBoneScale(FM3File);
end;

procedure TFMain.UpdateLabels;
begin
  lblStruct.Caption := Format('Structures File: "%s"',[FStructFileName]);
  lblLastFile.Caption := Format('Last Opened File: "%s"',[FCurrentFileName]);
end;

procedure TFMain.StructuresUpdate;
begin
  FM3File.ResetRefFrom;
  if FTagEditor <> nil then
    FTagEditor.ResetTagTree;
  if FAnimListForm <> nil then
    FAnimListForm.ResetAnimView;
end;

procedure TFMain.TryOpenFile(const FileName: string);
begin
  if (FileName = '') or not FileExists(FileName) then Exit;
  Log('Opening "%s"',[FileName]);
  if IsValidM3File(FileName) then
  begin
    FCurrentFileName := FileName;
    FM3File.LoadM3File(FileName);
    FModified := false;
    Log('%d tags loaded from "%s"',[FM3File.TagCount, FileName]);
  end
  else
  begin
    Log('Parsing M3ML file: "%s"',[FileName]);
    ImportFromM3ML(FM3File, FileName);
    FModified := true;
    FCurrentFileName := '';
  end;
  StructuresUpdate;
  UpdateLabels;
end;

procedure TFMain.InitGL(var rActive: Boolean);
begin
  rActive := InitOpenGL;
  if not rActive then
  begin
    Log('OpenGL initialization failed!');
    Exit;
  end;
  if not ImplementationRead then
    ReadImplementationProperties;
  if not ExtensionsRead then
    ReadExtensions;
end;

procedure TFMain.FrameStartGL(var rActive: Boolean);
begin
  if Assigned(F3DViewForm) then F3DViewForm.FrameStart;
end;

procedure TFMain.RenderGL(var rActive: Boolean);
begin
  case GetCurrentRenderWindowIndex of
    0: if Assigned(F3DViewForm) then F3DViewForm.FrameRender;
  end;
end;

procedure TFMain.Log(const S: string);
begin
  if Assigned(FOnLog) then
    FOnLog(S)
  else
  begin
    if MemoLog.Lines.Count >= 10000 then
      MemoLog.Lines.Clear;
    MemoLog.Lines.Add(FormatDateTime('[hh:mm:ss] ',Now)+S);
  end;
end;

procedure TFMain.Log(const Fmt: string; const Args: array of const);
begin
  if Assigned(FOnLog) then
    FOnLog(Format(Fmt,Args))
  else
  begin
    if MemoLog.Lines.Count >= 10000 then
      MemoLog.Lines.Clear;
    MemoLog.Lines.Add(FormatDateTime('[hh:mm:ss] ',Now)+Fmt,Args);
  end;
end;

procedure TFMain.FreeTagEditor;
begin
  if FTagEditor <> nil then
    FTagEditor := nil;
  btnTreeViewEditor.Enabled := true;
  BringToFront;
end;

procedure TFMain.FreeAnimListForm;
begin
  if FAnimListForm <> nil then
    FAnimListForm := nil;
  BAminList.Enabled := true;
  BringToFront;
end;

procedure TFMain.Free3DViewForm;
begin
  if F3DViewForm <> nil then
    F3DViewForm := nil;
  BringToFront;
end;

procedure TFMain.ModelChanged(const Changer: TForm);
begin
  FModified := true;
  FM3File.ResetRefFrom;
  if (FTagEditor <> nil) and (Changer <> FTagEditor) then
    FTagEditor.ResetTagTree;
  if (FAnimListForm <> nil) and (Changer <> FAnimListForm) then
    FAnimListForm.ResetAnimView;
end;

end.

