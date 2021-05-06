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
unit RenderEx;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, dglOpenGL;

function LoadShader(const FileName: string; const ShaderType: GLuint; out Shader: GLHandle): boolean;
function LoadPorgram(const VertexShader, FragmentShader: string; out ShaderProgram: GLHandle): boolean;

function GetLastShaderLog: string;

implementation

var
  logStr: string;

function LoadShader(const FileName: string; const ShaderType: GLuint;
  out Shader: GLHandle): boolean;
var
  s: string;
  ps: PChar;
  i: GLint;
  pch: array[word] of char;
begin
  Result := false;
  if not FileExists(FileName) then
  begin
    logStr := logStr + 'File not found "'+FileName+'"' + #13#10;
    Exit;
  end;
  with TStringList.Create do
  try
    LoadFromFile(FileName);
    s := Text;
  finally
    Free;
  end;
  if Trim(s)='' then exit;

  Shader := glCreateShader(ShaderType);
  s := s+#0;
  i := length(s);
  ps := @s[1];
  glShaderSource(Shader,1,@ps,@i);
  glCompileShader(Shader);

  glGetShaderiv(Shader,GL_COMPILE_STATUS,@i);
  Result := (i <> 0);
  glGetShaderiv(Shader,GL_INFO_LOG_LENGTH,@i);
  if i > 0 then
  begin
    glGetShaderInfoLog(Shader,$FFFE,@i,@pch[0]);
    pch[i] := #0;
    logStr := logStr + pch + #13;
  end;
  if not Result then
  begin
    logStr := logStr + 'Failed to make shader from "'+FileName+'"' + #13#10;
    glDeleteShader(Shader);
    Shader := 0;
  end;
end;

function LoadPorgram(const VertexShader, FragmentShader: string; out
  ShaderProgram: GLHandle): boolean;
var
  vert, frag: GLHandle;
  i: GLint;
  pch: array[word] of char;
begin
  Result := false;
  if LoadShader(VertexShader,GL_VERTEX_SHADER,vert) and LoadShader(FragmentShader,GL_FRAGMENT_SHADER,frag) then
  begin
    ShaderProgram := glCreateProgram();
    glAttachShader(ShaderProgram,vert);
    glAttachShader(ShaderProgram,frag);
    glLinkProgram(ShaderProgram);

    glGetProgramiv(ShaderProgram,GL_LINK_STATUS,@i);
    Result := (i <> 0);
    glGetProgramiv(ShaderProgram,GL_INFO_LOG_LENGTH,@i);
    if i > 0 then
    begin
      glGetProgramInfoLog(ShaderProgram,$FFFE,@i,@pch[0]);
      pch[i] := #0;
      logStr := logStr + pch + #13#10;
    end;

    glDetachShader(ShaderProgram,vert);
    glDetachShader(ShaderProgram,frag);
    glDeleteShader(vert);
    glDeleteShader(frag);
    if not Result then
    begin
      logStr := logStr + 'Failed to make program' + #13#10;
      glDeleteProgram(ShaderProgram);
      ShaderProgram := 0;
    end
  end;
end;

function GetLastShaderLog: string;
begin
  Result := logStr;
  logStr := '';
end;

end.

