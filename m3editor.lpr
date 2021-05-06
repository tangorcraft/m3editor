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
program m3editor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, umain, ustructures, uTagEditor, uM3File, uEditString, uEditInteger,
  uEditFlags, uEditFloat, uEditWord, uEditByte, uCHARBulkEdit, uM3ML, uCommon,
  uRefEdit, uNewTag, uColorEditor, uAbout, u3DViewForm, dglOpenGL, glMathUtils,
  glmCameraUtils, RenderUtils, CountersUtils, InterLockedSyncUtils,
  RenderEx, uToolBatchScan, uToolTextureRename;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.MainFormOnTaskBar := True;
  Application.CreateForm(TFMain, FMain);
  Application.Run;
end.

