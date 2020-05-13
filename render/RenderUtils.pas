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
unit RenderUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dglOpenGL;

type
  TRenderProc = procedure (var Active: Boolean);
  TRenderMethod = procedure (var Active: Boolean) of object;

procedure SetRenderMinDelay(const us: Double);
procedure SetMaxFPS(const FPS: Integer);

procedure RenderLock;
function TryRenderLock: Boolean;
procedure RenderUnLock;

function GetCurrentRenderTiming: double;

procedure AddRenderProc(const rProc: TRenderProc; const rIndex: Integer);
procedure AddRenderProc(const rMethod: TRenderMethod; const rIndex: Integer);
procedure ClearRenderList;
procedure FreeRenderList;

procedure SetRenderEventProc(const rProc: TRenderProc; const rEvent: Integer);
procedure SetRenderEventProc(const rMethod: TRenderMethod; const rEvent: Integer);

(*
While Render Thread active:
	RENDER_EVENT_INIT_WND
	if RENDER_EVENT_INIT_GL return True: While Rendering Active:
		Rendering Active := RENDER_EVENT_FRAME_START_WND
		Rendering Active := RENDER_EVENT_FRAME_START_GL
		For each window with RENDER_EVENT_SELECT_WND return true:
			if RENDER_EVENT_BEGIN_WND and RENDER_EVENT_BEGIN_GL return True:
				Loop through RenderList if not empty
				glFlush()
				RENDER_EVENT_END_GL
			RENDER_EVENT_END_WND
		RENDER_EVENT_FRAME_END_WND <- event handler loops through windows and do SwapBuffers
		RENDER_EVENT_FRAME_END_GL
	RENDER_EVENT_FINISH_GL
	RENDER_EVENT_FINISH_WND
*)

const
  RENDER_EVENT_INIT_WND        =  0;
  RENDER_EVENT_FINISH_WND      =  1;
  RENDER_EVENT_INIT_GL         =  2;
  RENDER_EVENT_FINISH_GL       =  3;
  RENDER_EVENT_FRAME_START_WND =  4;
  RENDER_EVENT_FRAME_END_WND   =  5;
  RENDER_EVENT_FRAME_START_GL  =  6;
  RENDER_EVENT_FRAME_END_GL    =  7;
  RENDER_EVENT_BEGIN_WND       =  8;
  RENDER_EVENT_END_WND         =  9;
  RENDER_EVENT_SELECT_WND      = 10;
  RENDER_EVENT_BEGIN_GL        = 11;
  RENDER_EVENT_END_GL          = 12;

implementation

uses
  CountersUtils, InterLockedSyncUtils;

const
  RENDER_EV_MIN =  0;
  RENDER_EV_MAX = 12;

type
  PRenderListRec = ^TRenderListRec;
  TRenderListRec = record
    Index: Integer;
    Active: Boolean;
    case IsProc: Boolean of
      True: (Proc: TRenderProc);
      False: (Method: TRenderMethod);
  end;

var
  renderPeriod: Double;

  renderList: TFPList;
  renderEvents: array[RENDER_EV_MIN..RENDER_EV_MAX] of TRenderListRec;

  ThreadActive: Boolean;
  ThreadHandle: THandle;
  ThreadId: THandle;

  rCS: TILCriticalSection;

  rTiming: double;

procedure RenderLock;
begin
  while not ILO_CriticalSectionTryEnter(rCS) do Sleep(1);
end;

function TryRenderLock: Boolean;
begin
  Result:=ILO_CriticalSectionTryEnter(rCS);
end;

procedure RenderUnLock;
begin
  ILO_CriticalSectionLeave(rCS);
end;

function GetCurrentRenderTiming: double;
begin
  Result:=rTiming;
end;

procedure AddRenderProc(const rProc: TRenderProc; const rIndex: Integer);
var
  p: PRenderListRec;
  i: integer;
begin
  RenderLock;
  try
    if not Assigned(rProc) then
      Exit;
    if not Assigned(renderList) then
      renderList:=TFPList.Create;
    New(p);
    p^.Index:=rIndex;
    p^.IsProc:=True;
    p^.Proc:=rProc;
    p^.Active:=True;

    i:=0;
    while (i < renderList.Count) and (PRenderListRec(renderList[i])^.Index < rIndex)  do
      inc(i);
    if i=renderList.Count then
      renderList.Add(p)
    else
      renderList.Insert(i,p);
  finally
    RenderUnLock;
  end;
end;

procedure AddRenderProc(const rMethod: TRenderMethod; const rIndex: Integer);
var
  p: PRenderListRec;
  i: integer;
begin
  RenderLock;
  try
    if not Assigned(rMethod) then
      Exit;
    if not Assigned(renderList) then
      renderList:=TFPList.Create;
    New(p);
    p^.Index:=rIndex;
    p^.IsProc:=False;
    p^.Method:=rMethod;
    p^.Active:=True;

    i:=0;
    while (i < renderList.Count) and (PRenderListRec(renderList[i])^.Index < rIndex)  do
      inc(i);
    if i=renderList.Count then
      renderList.Add(p)
    else
      renderList.Insert(i,p);
  finally
    RenderUnLock;
  end;
end;

procedure ClearRenderList;
var
  i: Integer;
  p: PRenderListRec;
begin
  RenderLock;
  try
    for i:=0 to renderList.Count - 1 do
    begin
      p:=renderList[i];
      Dispose(p);
    end;
    renderList.Clear;
  finally
    RenderUnLock;
  end;
end;

procedure FreeRenderList;
var
  i: Integer;
  p: PRenderListRec;
begin
  RenderLock;
  try
    for i:=0 to renderList.Count - 1 do
    begin
      p:=renderList[i];
      Dispose(p);
    end;
    renderList.Free;
    renderList:=nil;
  finally
    RenderUnLock;
  end;
end;

procedure CleanRenderList;
{ dispose render functions that returned false or raised exception }
var
  i: Integer;
  p: PRenderListRec;
begin
  RenderLock;
  try
    if Assigned(renderList) then
    begin
      i := 0;
      while i < renderList.Count do
      begin
        p:=renderList[i];
        if not p^.Active then
        begin
          renderList.Delete(i);
          Dispose(p);
        end
        else
          Inc(i);
      end;
    end;
  finally
    RenderUnLock;
  end;
end;

procedure SetRenderEventProc(const rProc: TRenderProc; const rEvent: Integer);
begin
  if (rEvent < RENDER_EV_MIN) or (rEvent > RENDER_EV_MAX) then
    Exit;
  RenderLock;
  try
    with renderEvents[rEvent] do
    begin
      if Assigned(rProc) then
      begin
        Index:=0;
        IsProc:=True;
        Proc:=rProc;
        Active:=True;
      end
      else
        Active:=False;
    end;
  finally
    RenderUnLock;
  end;
end;

procedure SetRenderEventProc(const rMethod: TRenderMethod; const rEvent: Integer);
begin
  if (rEvent < RENDER_EV_MIN) or (rEvent > RENDER_EV_MAX) then
    Exit;
  RenderLock;
  try
    with renderEvents[rEvent] do
    begin
      if Assigned(rMethod) then
      begin
        Index:=0;
        IsProc:=False;
        Method:=rMethod;
        Active:=True;
      end
      else
        Active:=False;
    end;
  finally
    RenderUnLock;
  end;
end;

function RunEventMethod(const rEvent: Integer; const Def: Boolean): Boolean;
begin
  try
    Result:=Def;
    RenderLock;
    try
      with renderEvents[rEvent] do
        if Active then
        begin
          if IsProc then
            Proc(Result)
          else
            Method(Result);
        end;
    finally
      RenderUnLock;
    end;
  except
    Result:=False;
  end;
end;

function RenderThread(Parameter: Pointer): IntPtr;
var
  rActive: Boolean;

  rNextTiming: Double;
  rPeriod: Double;

  i: Integer;

  rList: PPointerList;
  rListCnt: integer;
  rListCap: integer;

begin
  Result:=0;
  rActive := False;
  rNextTiming:=0;
  rPeriod:=0;
  //SetThreadPriority(ThreadHandle,THREAD_PRIORITY_ABOVE_NORMAL);
  rList:=nil;
  rListCnt:=0;
  rListCap:=0;
  try
    while ThreadActive do
    begin
      rActive:=RunEventMethod(RENDER_EVENT_INIT_WND, False);

      if rActive and RunEventMethod(RENDER_EVENT_INIT_GL, True) then
      begin
        while rActive and ThreadActive do
        begin
          if rPeriod<>renderPeriod then
          begin
            rPeriod:=renderPeriod;
            rNextTiming:=GetMicroCounter;
          end;
          rTiming := GetMicroCounter;

          if rTiming > rNextTiming then
          begin
            rNextTiming := rNextTiming + rPeriod;
            if rTiming > rNextTiming then
              rNextTiming := rTiming + rPeriod;

            rActive:=RunEventMethod(RENDER_EVENT_FRAME_START_WND, False);
            if rActive then
            begin
              rActive:=RunEventMethod(RENDER_EVENT_FRAME_START_GL, True);
              RenderLock;
              try
                while ThreadActive and RunEventMethod(RENDER_EVENT_SELECT_WND, False) do
                begin
                  { rendering begin here }
                  rTiming := GetMicroCounter;
                  if RunEventMethod(RENDER_EVENT_BEGIN_WND, False) and RunEventMethod(RENDER_EVENT_BEGIN_GL, True) then
                  begin
                    if Assigned(renderList) then
                    begin
                      if rListCap<renderList.Count then
                      begin
                        rListCap:=renderList.Count;
                        ReallocMem(rList, SizeOf(Pointer)*rListCap);
                      end;
                      rListCnt:=renderList.Count;
                      if rListCnt>0 then
                      begin
                        system.Move(renderList.List^, rList^, SizeOf(Pointer)*rListCnt);
                        for i := 0 to rListCnt-1 do
                          with PRenderListRec(rList^[i])^ do
                          try
                            if IsProc then
                              Proc(Active)
                            else
                              Method(Active);
                          except
                            Active:=False;
                          end;
                      end;
                    end;
                    glFlush();
                    { rendering end here }
                    rTiming := GetMicroCounter;
                    RunEventMethod(RENDER_EVENT_END_GL, False);
                  end;
                  RunEventMethod(RENDER_EVENT_END_WND, False);
                end;
              finally
                RenderUnLock;
              end;
              rTiming := rNextTiming;
              rActive := RunEventMethod(RENDER_EVENT_FRAME_END_WND, False);
              RunEventMethod(RENDER_EVENT_FRAME_END_GL, False);
              CleanRenderList;
            end;
          end;
          Sleep(1);
        end;
        RunEventMethod(RENDER_EVENT_FINISH_GL, False);
      end;
      RunEventMethod(RENDER_EVENT_FINISH_WND, False);
      Sleep(100);
    end;
  finally
    ReallocMem(rList, 0);
    CloseThread(ThreadHandle);
    //CloseHandle(ThreadHandle);
    if ThreadActive then
    begin
      ThreadHandle:=BeginThread(nil, 0, @RenderUtils.RenderThread, Parameter, 0, ThreadId);
    end;
  end;
end;

procedure SetRenderMinDelay(const us: Double);
begin
  if us < 0 then
    renderPeriod:=0
  else
    renderPeriod:=us;
end;

procedure SetMaxFPS(const FPS: Integer);
begin
  if FPS <= 0 then
    renderPeriod := MICRO_CNT_SEC
  else
    renderPeriod := MICRO_CNT_SEC / FPS;
end;

initialization
  ILO_CriticalSectionInit(rCS);

  renderPeriod := MICRO_CNT_SEC / 30; // default 30fps max

  ThreadActive:=True;

  ThreadHandle:=BeginThread(
    nil,
    0,
    @RenderThread,
    nil,
    0,
    ThreadId
  );

finalization
  ThreadActive:=False;
  while ILO_CriticalSectionLeave(rCS)<>nil do sleep(0);

end.

