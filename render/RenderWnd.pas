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
unit RenderWnd;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, dglOpenGL, RenderUtils;

type
  TProcedureOfObject = procedure of object;

function CreateRenderWindow(const wParent: HWND; const wIdx: Integer; const InitProc: TProcedure = nil; FinalProc: TProcedure = nil): boolean;
function CreateRenderWindow(const wParent: HWND; const wIdx: Integer; const InitProc, FinalProc: TProcedureOfObject): boolean;
function RenderWindowResized(var W, H: Integer; const wIdx: Integer): Boolean;
function RenderWindowVisible(const wIdx: Integer): Boolean;

function GetCurrentRenderWindowIndex: Integer;

function GetWndListCount: Integer;
procedure SetRenderWindowsListCount(const NewCount: Integer);

procedure WndRenderStart;
procedure WndRenderStop;
procedure WaitForFrameRenderEnd;

implementation

const
  WndClassNameW: array[0..11] of WideChar = ('g','l','R','e','n','d','e','r','W','n','d',#0);

type
  PRenderWndRec = ^TRenderWndRec;
  TRenderWndRec = record
    Handle: HWND;
    initDone: Boolean;

    rendering: Boolean;
    curW, curH: Longint;

    rDC: HDC;
    rRC: HGLRC;

    finProc: TProcedure;
    finMethod: TProcedureOfObject;
  end;

var
  ClassAtom: ATOM;

  wndList: array of TRenderWndRec;

  curWnd: Integer;
  wListCount: Integer;

  renderActive: Boolean;
  renderingFrame: Boolean;

function GetCurrentRenderWindowIndex: Integer;
begin
  Result:=curWnd;
end;

function GetWndListCount: Integer;
begin
  Result:=wListCount;
end;

procedure SetRenderWindowsListCount(const NewCount: Integer);
var
  rep: Boolean;
  i: Integer;
begin
  if GetCurrentThreadId<>MainThreadID then
    Exit;
  if NewCount=wListCount then
    Exit;
  rep:=True;
  while rep do
  begin
    RenderLock;
    try
      if NewCount=wListCount then
        Exit;
      if not renderingFrame then
      begin
        rep:=False;

        if NewCount>wListCount then
        begin
          SetLength(wndList,NewCount);
          for i := wListCount to NewCount-1 do
          with wndList[i] do
          begin
            Handle:=INVALID_HANDLE_VALUE;
            initDone:=False;
            rendering:=False;
          end;
        end
        else
        begin
          for i := NewCount to wListCount-1 do
            DestroyWindow(wndList[i].Handle);
          SetLength(wndList,NewCount);
        end;
        wListCount:=NewCount;
      end;
    finally
      RenderUnLock;
    end;
    if rep then
      Sleep(1);
  end;
end;

procedure WndRenderStart;
begin
  if not renderActive then
    renderActive := True;
end;

procedure WndRenderStop;
begin
  if not renderActive or (GetCurrentThreadId<>MainThreadID) then
    Exit;
  while renderActive do
  begin
    RenderLock;
    try
      if not renderingFrame then
        renderActive := False;
    finally
      RenderUnLock;
    end;
    if renderActive then
      Sleep(1);
  end;
end;

procedure WaitForFrameRenderEnd;
var
  rep: Boolean;
begin
  if GetCurrentThreadId<>MainThreadID then
    Exit;
  rep:=True;
  while rep do
  begin
    RenderLock;
    try
      rep := renderingFrame;
    finally
      RenderUnLock;
    end;
    if rep then
      Sleep(1);
  end;
end;

function GetWndIdxByHandle(const wnd: HWND): Integer;
var
  i: Integer;
begin
  for i:=0 to wListCount-1 do
    if wndList[i].Handle=wnd then
    begin
      Result:=i;
      Exit;
    end;
  Result:=-1;
end;

function WindowProc(wHandle: HWnd; uMsg: UINT; WParam: wParam;
  LParam: lParam): lResult; stdcall;
var
  wP: HWND;
  R: TRect;
  wIdx: Integer;
  rep: Boolean;
begin
  wIdx:=GetWndIdxByHandle(wHandle);
  case uMsg of
    WM_NCHITTEST:
      begin
        Result := DefWindowProc(wHandle, uMsg, WParam, LParam);
        if (Result <> HTNOWHERE)and(Result<>HTERROR) then
          Result:=HTTRANSPARENT;
      end;
    WM_ERASEBKGND:
      begin
        Result := 1;
      end;
    WM_DESTROY:
      begin
        if (wIdx>=0) and (wIdx < wListCount) then
        with wndList[wIdx] do
        begin
          if initDone then
          begin
            rep:=True;
            while rep do
            begin
              RenderLock;
              try
                if not rendering then
                begin
                  if wglMakeCurrent(rDC,rRC) then
                  try
                    if Assigned(finProc) then finProc();
                    if Assigned(finMethod) then finMethod();
                  except

                  end;
                  wglMakeCurrent(0,0);
                  DestroyRenderingContext(rRC);
                  ReleaseDC(Handle,rDC);
                  initDone:=False;
                  Handle:=INVALID_HANDLE_VALUE;
                  rep:=False;
                end;
              finally
                RenderUnLock;
              end;
              if rep then
                Sleep(1);
            end;
            // restart render ?
          end
          else
            Handle:=INVALID_HANDLE_VALUE;
        end;
        Result := DefWindowProc(wHandle, uMsg, WParam, LParam);
      end;
    else Result := DefWindowProc(wHandle, uMsg, WParam, LParam);;
  end; // case
end;

procedure InitWndClass;
var
  WindowClass : WNDCLASSW;
begin
  if ClassAtom <> 0 then Exit;
  with WindowClass do begin
    style := CS_OWNDC;
    lpfnWndProc := @WindowProc;
    cbClsExtra := 0;
    cbWndExtra := 0;
    hInstance := system.HInstance;
    hIcon := Windows.LoadIcon(0, IDI_APPLICATION);;
    hCursor := LoadCursor (0, idc_Arrow);   {Ссылка на курсор, сейчас - в виде стрелки.}
    hbrBackground := 0;
    lpszMenuName := nil;
    lpszClassName := @WndClassNameW; {Имя класса.}
  end;
  ClassAtom:= Windows.RegisterClassW(@WindowClass);
  if ClassAtom=0 then
    raise Exception.CreateFmt('Couldn''t create a window class.'#13'Error code: %d',
      [GetLastError]);
end;

function CreateRenderWindowInternal(const wParent: HWND; const wIdx: Integer): Boolean;
var
  R: TRect;
begin
  Result:=false;
  if (wIdx >= 0) and (wIdx < wListCount) then
  with wndList[wIdx] do
  begin
    if IsWindow(Handle) then
    begin
      Result:=true;
      if IsWindow(wParent) and (wParent <> 0) then
        SetParent(Handle,wParent);
      Exit;
    end;
    FillChar(R,SizeOf(R),0);
    if IsWindow(wParent) and (wParent<>0) and GetClientRect(wParent,R) then
    begin
      InitWndClass;
      Handle := Windows.CreateWindowExW(
        0,
        @WndClassNameW,
        PWideChar(WideString('')),   {Заголовок окна}
        WS_CHILD or WS_VISIBLE,      {Стиль окна.}
        R.Left,  {X}
        R.Top,   {Y}
        R.Right, {Width}
        R.Bottom,{Height}
        wParent,     {WndParent - родительское окно}
        0,           {Menu}
        HInstance,   {Instance}
        nil
      );
      if Handle = 0 then
        Handle:=INVALID_HANDLE_VALUE
      else
      begin
        rDC:=GetDC(Handle);
        rRC:=CreateRenderingContext(rDC,[opDoubleBuffered],32,24,0,0,0,0);
        initDone:=rRC<>0;
        if not initDone then
        begin
          DestroyWindow(Handle);
          Handle:=INVALID_HANDLE_VALUE;
        end
        else
        begin
          Result:=true;
          if wglMakeCurrent(rDC,rRC) then
          begin
            ReadImplementationProperties;
            ReadExtensions;
            wglMakeCurrent(0,0);
          end;
          rendering:=False;
          curW:=R.Right;
          curH:=R.Bottom;
        end;
      end;
    end;
  end;
end;

function CreateRenderWindow(const wParent: HWND; const wIdx: Integer;
  const InitProc: TProcedure = nil; FinalProc: TProcedure = nil): boolean;
begin
  Result := CreateRenderWindowInternal(wParent,wIdx);
  if Result and Assigned(InitProc) then
  begin
    with wndList[wIdx] do
    if wglMakeCurrent(rDC,rRC) then
    begin
      finProc := FinalProc;
      finMethod := nil;
      InitProc();
      wglMakeCurrent(0,0);
    end;
  end;
end;

function CreateRenderWindow(const wParent: HWND; const wIdx: Integer;
  const InitProc, FinalProc: TProcedureOfObject): boolean;
begin
  Result := CreateRenderWindowInternal(wParent,wIdx);
  if Result and Assigned(InitProc) then
  begin
    with wndList[wIdx] do
    if wglMakeCurrent(rDC,rRC) then
    begin
      finProc := nil;
      finMethod := FinalProc;
      InitProc;
      wglMakeCurrent(0,0);
    end;
  end;
end;

function RenderWindowResized(var W, H: Integer; const wIdx: Integer): Boolean;
begin
  Result:=False;
  if (wIdx>=0) and (wIdx < wListCount) then
  with wndList[wIdx] do
    if initDone and ((curW<>W)or(curH<>H)) then
    begin
      W:=curW;
      H:=curH;
      Result:=True;
    end;
end;

function RenderWindowVisible(const wIdx: Integer): Boolean;
var
  R: TRect;
  i: integer;
begin
  Result:=false;
  if (wIdx>=0) and (wIdx < wListCount) then
  with wndList[wIdx] do
    if initDone and IsWindowVisible(Handle) then
    begin
      i := GetClipBox(rDC,R);
      Result := (i=SIMPLEREGION)or(i=COMPLEXREGION);
    end;
end;

procedure UpdateRenderWindowSize(const wIdx: Integer);
var
  wP: HWND;
  R: TRect;
  W,H: Integer;
begin
  if (wIdx>=0) and (wIdx < wListCount) then
  with wndList[wIdx] do
    if initDone then
    begin
      FillChar(R,SizeOf(R),0);
      GetWindowRect(Handle,R);
      W:=r.Right-r.Left;
      H:=r.Bottom-r.Top;

      wP:=GetAncestor(Handle,GA_PARENT);
      FillChar(R,SizeOf(R),0);
      if IsWindow(wP) and GetClientRect(wP,R) and ((R.Right<>W)or(R.Bottom<>H)) then
      begin
        W:=R.Right;
        H:=R.Bottom;
        MoveWindow(Handle,0,0,W,H,False);
      end;
      if (curW<>W)or(curH<>H) then
      begin
        curW:=W;
        curH:=H;
      end;
    end;
end;

procedure RenderInit(var Active: Boolean);
var
  i: integer;
begin
  Active:=False;
  renderingFrame:=False;
  if renderActive then
  begin
    for i := 0 to wListCount-1 do
    with wndList[i] do
    begin
      if initDone and IsWindow(Handle) then
      begin
        if not Active then
          Active:=True;
        Exit;
      end;
    end;
  end;
end;

procedure RenderDone(var Active: Boolean);
begin
  renderingFrame:=False;
  // need to do something?
end;

procedure RenderFrameStart(var Active: Boolean);
begin
  Active:=False;
  if renderActive and (wListCount>0) then
  begin
    curWnd:=0;
    Active:=True;
    renderingFrame:=True;
  end;
end;

procedure RenderWndSelect(var Active: Boolean);
begin
  Active:=False;
  if renderActive then
  begin
    while not Active and (curWnd < wListCount) do
    with wndList[curWnd] do
    begin
      if initDone and wglMakeCurrent(rDC,rRC) then
      begin
        Active:=True;
        rendering:=True;
        if not ImplementationRead then
          ReadImplementationProperties;
        if not ExtensionsRead then
          ReadExtensions;
      end
      else
        inc(curWnd);
    end;
  end;
  if Active then
    UpdateRenderWindowSize(curWnd);
end;

procedure RenderFrameEnd(var Active: Boolean);
var
  i: integer;
  p: PRenderWndRec;
begin
  Active:=renderActive;
  renderingFrame:=False;
  DeactivateRenderingContext;
  for i := 0 to wListCount-1 do
  with wndList[i] do
  begin
    if initDone then
      SwapBuffers(rDC);
  end;
end;

procedure RenderBegin(var Active: Boolean);
begin
  with wndList[curWnd] do
    Active := renderActive and initDone and IsWindow(Handle) and RenderWindowVisible(curWnd);
end;

procedure RenderEnd(var Active: Boolean);
begin
  wndList[curWnd].rendering := False;
  inc(curWnd);
end;

initialization
  ClassAtom := 0;
  wListCount:=1;
  SetLength(wndList,wListCount);
  SetRenderEventProc(@RenderInit,RENDER_EVENT_INIT_WND);
  SetRenderEventProc(@RenderDone,RENDER_EVENT_FINISH_WND);

  SetRenderEventProc(@RenderFrameStart,RENDER_EVENT_FRAME_START_WND);
  SetRenderEventProc(@RenderWndSelect,RENDER_EVENT_SELECT_WND);
  SetRenderEventProc(@RenderFrameEnd,RENDER_EVENT_FRAME_END_WND);

  SetRenderEventProc(@RenderBegin,RENDER_EVENT_BEGIN_WND);
  SetRenderEventProc(@RenderEnd,RENDER_EVENT_END_WND);
finalization

end.

