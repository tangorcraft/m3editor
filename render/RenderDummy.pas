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
unit RenderDummy;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dglOpenGL, RenderUtils;

type
  TProcedureOfObject = procedure of object;

function CreateRenderWindow(const wParent: THandle; const wIdx: Integer; const InitProc: TProcedure = nil; FinalProc: TProcedure = nil): boolean;
function CreateRenderWindow(const wParent: THandle; const wIdx: Integer; const InitProc, FinalProc: TProcedureOfObject): boolean;
function RenderWindowResized(var W, H: Integer; const wIdx: Integer): Boolean;
function RenderWindowVisible(const wIdx: Integer): Boolean;

function GetCurrentRenderWindowIndex: Integer;

function GetWndListCount: Integer;
procedure SetRenderWindowsListCount(const NewCount: Integer);

procedure WndRenderStart;
procedure WndRenderStop;
procedure WaitForFrameRenderEnd;

implementation

var
  wListCount: Integer;

function GetCurrentRenderWindowIndex: Integer;
begin
  Result:=0;
end;

function GetWndListCount: Integer;
begin
  Result:=wListCount;
end;

procedure SetRenderWindowsListCount(const NewCount: Integer);
begin
  wListCount:=NewCount;
end;

procedure WndRenderStart;
begin

end;

procedure WndRenderStop;
begin

end;

procedure WaitForFrameRenderEnd;
begin
  Sleep(1);
end;

function GetWndIdxByHandle(const wnd: HWND): Integer;
begin
  Result:=-1;
end;

function CreateRenderWindow(const wParent: THandle; const wIdx: Integer;
  const InitProc: TProcedure; FinalProc: TProcedure): boolean;
begin
  Result := false;
end;

function CreateRenderWindow(const wParent: THandle; const wIdx: Integer;
  const InitProc, FinalProc: TProcedureOfObject): boolean;
begin
  Result := false;
end;

function RenderWindowResized(var W, H: Integer; const wIdx: Integer): Boolean;
begin
  Result:=False;
end;

function RenderWindowVisible(const wIdx: Integer): Boolean;
begin
  Result:=false;
end;

procedure RenderInit(var Active: Boolean);
begin
  Active:=False;
end;

procedure RenderDone(var Active: Boolean);
begin
  Active:=False;
end;

procedure RenderFrameStart(var Active: Boolean);
begin
  Active:=False;
end;

procedure RenderWndSelect(var Active: Boolean);
begin
  Active:=False;
end;

procedure RenderFrameEnd(var Active: Boolean);
begin
  Active:=false;
end;

procedure RenderBegin(var Active: Boolean);
begin
  Active := false;
end;

procedure RenderEnd(var Active: Boolean);
begin
  Active := false;
end;

initialization
  SetRenderEventProc(@RenderInit,RENDER_EVENT_INIT_WND);
  SetRenderEventProc(@RenderDone,RENDER_EVENT_FINISH_WND);

  SetRenderEventProc(@RenderFrameStart,RENDER_EVENT_FRAME_START_WND);
  SetRenderEventProc(@RenderWndSelect,RENDER_EVENT_SELECT_WND);
  SetRenderEventProc(@RenderFrameEnd,RENDER_EVENT_FRAME_END_WND);

  SetRenderEventProc(@RenderBegin,RENDER_EVENT_BEGIN_WND);
  SetRenderEventProc(@RenderEnd,RENDER_EVENT_END_WND);
finalization

end.

