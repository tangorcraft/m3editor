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
    BMeshEditor: TButton;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MFileOpenClick(Sender: TObject);
    procedure MSaveAsClick(Sender: TObject);
    procedure MSaveClick(Sender: TObject);
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
    procedure ResetRefFrom;
  public
    procedure Log(const S: string);
    procedure Log(const Fmt : string; const Args : Array of const);

    procedure FreeTagEditor;
    procedure ModelChanged(const Changer: TForm);
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
  FModified := False;
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
  if FTagEditor = nil then
  begin
    Application.CreateForm(TFTagEditor,FTagEditor);
    FTagEditor.ShowEditor(FM3File,false);
    btnTreeViewEditor.Enabled := false;
  end;
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
var
  i: integer;
begin
  FTagEditor.Free;
  Structures.Free;
  FM3File.Free;
end;

procedure TFMain.MFileOpenClick(Sender: TObject);
var
  i: integer;
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
    FCurrentFileName := OpenDialog.FileName;
    FM3File.LoadM3File(OpenDialog.FileName);
    FModified := false;
    Log('%d tags loaded from "%s"',[FM3File.TagCount, OpenDialog.FileName]);
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
  if (FCurrentFileName = '') or (not DirectoryExists(ExtractFileDir(FCurrentFileName))) then Exit;
  if FileExists(FCurrentFileName) and (FCurrentFileName <> FLastSavedFileName) then
  begin
    if MessageDlg('Save file','Replace file "'+FCurrentFileName+'"?',mtConfirmation,mbOKCancel,0)<>mrOK then Exit;
  end;
  Log('Saving to "%s"',[FCurrentFileName]);
  FM3File.SaveM3File(FCurrentFileName);
  FModified := false;
  FLastSavedFileName := FCurrentFileName;
end;

procedure TFMain.MStructOpenClick(Sender: TObject);
begin
  if OpenStructDialog.Execute then
  begin
    FStructFileName := OpenStructDialog.FileName;
    Log('Loading structures info from "%s"',[FStructFileName]);
    Structures.LoadStructures(FStructFileName);
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
  ResetRefFrom;
end;

procedure TFMain.ResetRefFrom;
var
  i, j, k, idx: Integer;
  pRef: Pm3ref_small;
begin
  // this function have 5 (five) indexes and 3 (three) loops
  // stay strong and don't get lost
  for i := 0 to FM3File.TagCount - 1 do
    SetLength(FM3File[i]^.RefFrom,0);
  for i := 0 to FM3File.TagCount - 1 do
  begin
    Structures.GetStructureInfo(FM3File[i]^);
    with FM3File[i]^ do
    if ItemSize >= sizeof(m3ref_small) then
    begin
      for j := 0 to length(ItemFields)-1 do
      begin
        if (ItemFields[j].fTypeName = 'Reference') or (ItemFields[j].fTypeName = 'SmallReference') then
          for idx := 0 to ItemCount-1 do
          begin
            pRef := Data + (ItemSize*idx) + ItemFields[j].fOffset;
            with pRef^ do
            if (refCount > 0) and (refIndex > 0) and (refIndex < FM3File.TagCount) then
            begin
              k := length(FM3File[refIndex]^.RefFrom);
              SetLength(FM3File[refIndex]^.RefFrom, k+1);
              FM3File[refIndex]^.RefFrom[k].rfTagIndex := i;
              FM3File[refIndex]^.RefFrom[k].rfItemIndex := idx;
              FM3File[refIndex]^.RefFrom[k].frFieldRow := j + 1;
              FM3File[refIndex]^.RefFrom[k].rfRefFieldOffset := (ItemSize*idx) + ItemFields[j].fOffset;
              FM3File[refIndex]^.RefFrom[k].rfName := Format(
                '%d: %s [%d] -> %s (refCount = %d)',
                [i, FM3File[i]^.StructName, idx, ItemFields[j].fGroupName+ItemFields[j].fName, refCount]
              );
            end;
          end;
      end;
    end;
  end;
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
  ResetRefFrom;
  if (FTagEditor <> nil) and (Changer <> FTagEditor) then
    FTagEditor.ResetTagTree;
end;

end.

