(*
    Microseconds Counter
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
unit CountersUtils;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef MSWINDOWS}Windows,{$endif}
  {$ifdef LINUX}Linux, unixtype,{$endif}
  SysUtils;

const
  MICRO_CNT_MSEC = 1000.0;
  MICRO_CNT_SEC  = 1000.0 * MICRO_CNT_MSEC;

function GetMicroCounter: Double;

implementation

{$ifdef MSWINDOWS}
var
  PerfFreq: UInt64;
  PerfFactor: Extended;

procedure InitTimings;
begin
  QueryPerformanceFrequency(@PerfFreq);
  PerfFactor:=1000000/PerfFreq;
end;
{$endif}

function GetMicroCounter: Double;
{$ifdef MSWINDOWS}
var
  i: UInt64;
begin
  QueryPerformanceCounter(@i);
  Result:=i*PerfFactor;
{$endif}
{$ifdef LINUX}
var
  ts: TTimeSpec;
begin
  clock_gettime(CLOCK_MONOTONIC_RAW,@ts);
  Result:=ts.tv_sec*1000.0 + ts.tv_nsec/1000.0;
{$endif}
end;

{$ifdef MSWINDOWS}
initialization
  InitTimings;
{$endif}
end.

