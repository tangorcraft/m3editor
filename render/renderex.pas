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

function LoadShader(const FileName: string; const ShaderType: GLuint; out Shader: GLuint): boolean;
function LoadPorgram(const VertexShader, FragmentShader: string; out ShaderProgram: GLuint): boolean;

implementation

function LoadShader(const FileName: string; const ShaderType: GLuint;
  out Shader: GLuint): boolean;
var
  s: string;
  i: GLint;
begin
  Result := false;
  if not FileExists(FileName) then exit;
  with TStringList.Create do
  try
    LoadFromFile(FileName);
    s := Text;
  finally
    Free;
  end;
  if Trim(s)='' then exit;

  Shader := glCreateShader(ShaderType);
  i := length(s);
  glShaderSource(Shader,1,@s[1],@i);
  glCompileShader(Shader);

  glGetShaderiv(Shader,GL_COMPILE_STATUS,@i);
  Result := (i = GLint(GL_TRUE));
  if not Result then
  begin
    glDeleteShader(Shader);
    Shader := 0;
  end;
end;

function LoadPorgram(const VertexShader, FragmentShader: string; out
  ShaderProgram: GLuint): boolean;
var
  vert, frag: GLuint;
  i: GLint;
begin
  Result := false;
  if LoadShader(VertexShader,GL_VERTEX_SHADER,vert) and LoadShader(FragmentShader,GL_FRAGMENT_SHADER,frag) then
  begin
    ShaderProgram := glCreateProgram();
    glAttachShader(ShaderProgram,vert);
    glAttachShader(ShaderProgram,frag);
    glLinkProgram(ShaderProgram);

    glGetProgramiv(ShaderProgram,GL_LINK_STATUS,@i);
    Result := (i = GLint(GL_TRUE));

    glDetachShader(ShaderProgram,vert);
    glDetachShader(ShaderProgram,frag);
    glDeleteShader(vert);
    glDeleteShader(frag);
    if not Result then
    begin
      glDeleteProgram(ShaderProgram);
      ShaderProgram := 0;
    end
  end;
end;

end.

