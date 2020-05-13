(*
    Thread Syncronization Using InterLocked Functions
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
unit InterLockedSyncUtils;

{$ALIGN 8}
{$mode objfpc}{$H+}

interface

type
  TILSemaphore = record
    ILO: longint;
    Sem: Cardinal;
    SemMax: Cardinal;
  end;

  TILCriticalSection = record
    ILO: Pointer;
    ReEnterCount: Cardinal;
  end;

procedure ILO_SemaphoreInit(var ILSem: TILSemaphore; const SemMax: Cardinal = 1; const SemInit: Cardinal = 0);
function ILO_SemaphoreTryGet(var ILSem: TILSemaphore): Boolean;
function ILO_SemaphoreTryGetWithSpin(var ILSem: TILSemaphore; SpinCount: Cardinal): Boolean;
procedure ILO_SemaphoreRelease(var ILSem: TILSemaphore);
procedure ILO_SemaphoreReleaseCount(var ILSem: TILSemaphore; const ReleaseCount: Cardinal);

procedure ILO_CriticalSectionInit(var ILCS: TILCriticalSection);
function ILO_CriticalSectionTryEnter(var ILCS: TILCriticalSection): Boolean;
function ILO_CriticalSectionTryEnterWithSpin(var ILCS: TILCriticalSection; SpinCount: Cardinal): Boolean;
function ILO_CriticalSectionLeave(var ILCS: TILCriticalSection): Pointer;

implementation

uses sysutils;

{ *** Semaphore *** }

procedure ILO_SemaphoreInit(var ILSem: TILSemaphore; const SemMax: Cardinal = 1; const SemInit: Cardinal = 0);
begin
  if SemMax = 0 then
    ILSem.SemMax := 1
  else
    ILSem.SemMax := SemMax;
  if SemInit > ILSem.SemMax then
    ILSem.Sem := ILSem.SemMax
  else
    ILSem.Sem := SemInit;
  ILSem.ILO := 0;
end;

function ILO_SemaphoreTryGet(var ILSem: TILSemaphore): Boolean;
begin
  if InterlockedCompareExchange(ILSem.ILO,1,0)=0 then
  begin
    Result := ILSem.Sem > 0;
    if Result then
      dec(ILSem.Sem);
    InterLockedExchange(ILSem.ILO,0);
  end
  else
    Result := False;
end;

function ILO_SemaphoreTryGetWithSpin(var ILSem: TILSemaphore; SpinCount: Cardinal): Boolean;
begin
  while InterlockedCompareExchange(ILSem.ILO,1,0)<>0 do
  begin
    if SpinCount=0 then
      Exit(False)
    else
      dec(SpinCount);
  end;
  Result := ILSem.Sem > 0;
  if Result then
    dec(ILSem.Sem);
  InterLockedExchange(ILSem.ILO,0);
end;

{
procedure ILO_SemaphoreGet(var ILSem: TILSemaphore);
var
  got: boolean;
begin
  repeat
    while InterlockedCompareExchange(ILSem.ILO,1,0)<>0 do Sleep(0);
    got := ILSem.Sem > 0;
    if got then
    begin
      dec(ILSem.Sem);
      InterLockedExchange(ILSem.ILO,0);
    end
    else
    begin
      InterLockedExchange(ILSem.ILO,0);
      Sleep(1);
    end;
  until got;
end;

procedure ILO_SemaphoreGetWithSpin(var ILSem: TILSemaphore; SpinCount: Cardinal);
var
  got: boolean;
begin
  repeat
    while InterlockedCompareExchange(ILSem.ILO,1,0)<>0 do
    begin
      if SpinCount = 0 then
        Sleep(0)
      else
        dec(SpinCount);
    end;
    got := ILSem.Sem > 0;
    if got then dec(ILSem.Sem);
    InterLockedExchange(ILSem.ILO,0);
    if not got then
    begin
      if SpinCount = 0 then
        Sleep(1)
      else
        dec(SpinCount);
    end;
  until got;
end;
}

procedure ILO_SemaphoreRelease(var ILSem: TILSemaphore);
begin
  while InterlockedCompareExchange(ILSem.ILO,1,0)<>0 do Sleep(0);
  if ILSem.Sem < ILSem.SemMax then
    Inc(ILSem.Sem);
  InterLockedExchange(ILSem.ILO,0);
end;

procedure ILO_SemaphoreReleaseCount(var ILSem: TILSemaphore; const ReleaseCount: Cardinal);
begin
  while InterlockedCompareExchange(ILSem.ILO,1,0)<>0 do Sleep(0);
  if ReleaseCount >= (ILSem.SemMax - ILSem.Sem) then
    ILSem.Sem := ILSem.SemMax
  else
    Inc(ILSem.Sem,ReleaseCount);
  InterLockedExchange(ILSem.ILO,0);
end;

{ *** Critical Section *** }

procedure ILO_CriticalSectionInit(var ILCS: TILCriticalSection);
begin
  ILCS.ILO := nil;
  ILCS.ReEnterCount := 0;
end;

function ILO_CriticalSectionTryEnter(var ILCS: TILCriticalSection): Boolean;
var
  tid, lock: Pointer;
begin
  tid := Pointer(GetCurrentThreadId());
  lock := InterlockedCompareExchange(ILCS.ILO,tid,nil);
  if lock = nil then
  begin
    ILCS.ReEnterCount:=0;
    Result := True;
  end
  else if lock = tid then
  begin
    inc(ILCS.ReEnterCount);
    Result := True;
  end
  else
    Result := False;
end;

function ILO_CriticalSectionTryEnterWithSpin(var ILCS: TILCriticalSection; SpinCount: Cardinal): Boolean;
var
  tid, lock: Pointer;
begin
  tid := Pointer(GetCurrentThreadId());
  while true do
  begin
    lock := InterlockedCompareExchange(ILCS.ILO,tid,nil);
    if lock = nil then
    begin
      ILCS.ReEnterCount:=0;
      Exit(True);
    end
    else if lock = tid then
    begin
      inc(ILCS.ReEnterCount);
      Exit(True);
    end
    else if SpinCount=0 then
      Exit(False);
    dec(SpinCount);
  end;
end;

function ILO_CriticalSectionLeave(var ILCS: TILCriticalSection): Pointer;
var
  tid: Pointer;
begin
  tid := Pointer(GetCurrentThreadId());
  Result := InterlockedCompareExchange(ILCS.ILO,tid,tid);
  if Result = tid then
  begin
    if ILCS.ReEnterCount=0 then
    begin
      InterLockedExchange(ILCS.ILO,nil);
      Exit(nil);
    end
    else
      dec(ILCS.ReEnterCount);
  end;
  if Result = nil then
    ILCS.ReEnterCount := 0;
end;

end.
