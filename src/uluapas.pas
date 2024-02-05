{
   Double commander
   -------------------------------------------------------------------------
   Push some useful functions to Lua

   Copyright (C) 2016-2023 Alexander Koblov (alexx2000@mail.ru)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program. If not, see <http://www.gnu.org/licenses/>.
}

unit uLuaPas;

{$mode objfpc}{$H+}

interface

uses
  uDCUtils, Classes, SysUtils, Lua;

procedure RegisterPackages(L : Plua_State);
procedure SetPackagePath(L: Plua_State; const Path: String);
function LuaPCall(L : Plua_State; nargs, nresults : Integer): Boolean;
function ExecuteScript(const FileName: String; Args: array of String; var sErrorToReportIfAny:string): Boolean;
function ExecuteScriptString(Script: PAnsiChar): Boolean;

implementation

uses
  Forms, Dialogs, Clipbrd, LazUTF8, LCLVersion, uLng, DCOSUtils,
  DCConvertEncoding, fMain, uFormCommands, uOSUtils, uGlobs, uLog,
  uClipboard, uShowMsg, uLuaStd, uFindEx, uConvEncoding, uFileProcs,
  uFilePanelSelect, uMasks, LazFileUtils, Character, UnicodeData,
  fDialogBox, Extension, LCLProc, Types, uGlobsPaths, DCXmlConfig,
  uArchiveFileSourceUtil;

const
  VERSION_API = 1;

type
  PLDLGUserData = ^DLGUserData;
  DLGUserData = record
    L: Plua_State;
    FuncName: PAnsiChar;
    FuncRef: Integer;
    DataRef: Integer;
  end;

procedure luaPushSearchRec(L : Plua_State; Rec: PSearchRecEx);
begin
  lua_pushlightuserdata(L, Rec);
  lua_newtable(L);
  lua_pushinteger(L, Rec^.Time);
  lua_setfield(L, -2, 'Time');
  lua_pushinteger(L, Rec^.Size);
  lua_setfield(L, -2, 'Size');
  lua_pushinteger(L, Rec^.Attr);
  lua_setfield(L, -2, 'Attr');
  lua_pushstring(L, Rec^.Name);
  lua_setfield(L, -2, 'Name');
end;

function luaFindFirst(L : Plua_State) : Integer; cdecl;
var
  Path: String;
  Rec: PSearchRecEx;
begin
  New(Rec);
  Path:= lua_tostring(L, 1);
  if FindFirstEx(Path, fffPortable, Rec^) = 0 then
  begin
    Result:= 2;
    luaPushSearchRec(L, Rec);
  end
  else begin
    FindCloseEx(Rec^);
    lua_pushnil(L);
    Dispose(Rec);
    Result:= 1;
  end;
end;

function luaFindNext(L : Plua_State) : Integer; cdecl;
var
  Rec: PSearchRecEx;
begin
  Rec:= lua_touserdata(L, 1);
  if (Rec <> nil) and (FindNextEx(Rec^) = 0) then
  begin
    Result:= 2;
    luaPushSearchRec(L, Rec);
  end
  else begin
    lua_pushnil(L);
    Result:= 1;
  end;
end;

function luaFindClose(L : Plua_State) : Integer; cdecl;
var
  Rec: PSearchRecEx;
begin
  Rec:= lua_touserdata(L, 1);
  if Assigned(Rec) then
  begin
    FindCloseEx(Rec^);
    Dispose(Rec);
  end;
  Result:= 0;
end;

function luaSleep(L : Plua_State) : Integer; cdecl;
begin
  Result:= 0;
  Sleep(lua_tointeger(L, 1));
end;

function luaGetTickCount(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushinteger(L, GetTickCount64);
end;

function luaFileGetAttr(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushinteger(L, FileGetAttr(mbFileNameToNative(lua_tostring(L, 1))));
end;

function luaFileExists(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushboolean(L, mbFileExists(lua_tostring(L, 1)));
end;

function luaDirectoryExists(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushboolean(L, mbDirectoryExists(lua_tostring(L, 1)));
end;

function luaCreateDirectory(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushboolean(L, mbForceDirectory(lua_tostring(L, 1)));
end;

function luaCreateHardLink(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushboolean(L, CreateHardLink(lua_tostring(L, 1), lua_tostring(L, 2)));
end;

function luaCreateSymbolicLink(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushboolean(L, CreateSymLink(lua_tostring(L, 1), lua_tostring(L, 2)));
end;

function luaReadSymbolicLink(L : Plua_State) : Integer; cdecl;
var
  Path: String;
  Recursive: Boolean = False;
begin
  Result:= 1;
  Path:= lua_tostring(L, 1);
  if lua_isboolean(L, 2) then begin
    Recursive:= lua_toboolean(L, 2)
  end;
  if Recursive then
    Path:= mbReadAllLinks(Path)
  else begin
    Path:= ReadSymLink(Path);
  end;
  lua_pushstring(L, Path);
end;

function luaExtractFilePath(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushstring(L, ExtractFilePath(lua_tostring(L, 1)));
end;

function luaExtractFileDrive(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushstring(L, ExtractFileDrive(lua_tostring(L, 1)));
end;

function luaExtractFileName(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushstring(L, ExtractFileName(lua_tostring(L, 1)));
end;

function luaExtractFileExt(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushstring(L, ExtractFileExt(lua_tostring(L, 1)));
end;

function luaMatchesMask(L : Plua_State) : Integer; cdecl;
var
  FileName: String;
  FileMask: String;
  AOptions: TMaskOptions;
begin
  Result:= 1;
  FileName:= lua_tostring(L, 1);
  FileMask:= lua_tostring(L, 2);
  if lua_isnumber(L, 3) then
    AOptions:= TMaskOptions(Integer(lua_tointeger(L, 3)))
  else begin
    AOptions:= [];
  end;
  lua_pushboolean(L, MatchesMask(FileName, FileMask, AOptions));
end;

function luaMatchesMaskList(L : Plua_State) : Integer; cdecl;
var
  FileName: String;
  FileMask: String;
  AOptions: TMaskOptions;
  ASeparatorCharset: String;
begin
  Result:= 1;
  FileName:= lua_tostring(L, 1);
  FileMask:= lua_tostring(L, 2);
  if lua_isstring(L, 3) then
    ASeparatorCharset:= lua_tostring(L, 3)
  else begin
    ASeparatorCharset:= ';';
  end;
  if lua_isnumber(L, 4) then
    AOptions:= TMaskOptions(Integer(lua_tointeger(L, 4)))
  else begin
    AOptions:= [];
  end;
  lua_pushboolean(L, MatchesMaskList(FileName, FileMask, ASeparatorCharset, AOptions));
end;

function luaExtractFileDir(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushstring(L, ExtractFileDir(lua_tostring(L, 1)));
end;

function luaGetAbsolutePath(L : Plua_State) : Integer; cdecl;
var
  FileName, BaseDir: String;
begin
  Result:= 1;
  FileName:= lua_tostring(L, 1);
  BaseDir:= lua_tostring(L, 2);
  lua_pushstring(L, CreateAbsolutePath(FileName, BaseDir));
end;

function luaGetRelativePath(L : Plua_State) : Integer; cdecl;
var
  FileName, BaseDir: String;
begin
  Result:= 1;
  FileName:= lua_tostring(L, 1);
  BaseDir:= lua_tostring(L, 2);
  lua_pushstring(L, CreateRelativePath(FileName, BaseDir));
end;

function luaGetTempName(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushstring(L, GetTempName(GetTempFolderDeletableAtTheEnd));
end;

function utf8_next(L: Plua_State): Integer; cdecl;
var
  S: String;
  C: Integer;
  Len: size_t;
  P: PAnsiChar;
  Index: Integer;
begin
  P:= lua_tolstring(L, lua_upvalueindex(1), @Len);
  Index:= lua_tointeger(L, lua_upvalueindex(2));
  if (Index >= Integer(Len)) then Exit(0);

  P:= P + Index;
  C:= UTF8CodepointSize(P);

  // Partial UTF-8 character
  if (Index + C) > Len then Exit(0);

  SetString(S, P, C);

  lua_pushinteger(L, Index + C);
  lua_replace(L, lua_upvalueindex(2));

  lua_pushinteger(L, Index + 1);
  lua_pushstring(L, S);

  Result:= 2;
end;

function luaNext(L : Plua_State) : Integer; cdecl;
begin
  lua_pushvalue(L, 1);
  lua_pushnumber(L, 0);
  lua_pushcclosure(L, @utf8_next, 2);
  Result:= 1;
end;

function luaPos(L : Plua_State) : Integer; cdecl;
var
  Offset: SizeInt = 1;
  Search, Source: String;
begin
  Result:= 1;
  Search:= lua_tostring(L, 1);
  Source:= lua_tostring(L, 2);
  if lua_isnumber(L, 3) then begin
    Offset:= lua_tointeger(L, 3)
  end;
  lua_pushinteger(L, UTF8Pos(Search, Source, Offset));
end;

function luaCopy(L : Plua_State) : Integer; cdecl;
var
  S: String;
  Start, Count: PtrInt;
begin
  Result:= 1;
  S:= lua_tostring(L, 1);
  Start:= lua_tointeger(L, 2);
  Count:= lua_tointeger(L, 3);
  S:= UTF8Copy(S, Start, Count);
  lua_pushstring(L, S);
end;

function luaLength(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushinteger(L, UTF8Length(lua_tostring(L, 1)));
end;

function luaUpperCase(L : Plua_State) : Integer; cdecl;
var
  S: String;
begin
  Result:= 1;
  S:= lua_tostring(L, 1);
  S:= UTF8UpperCase(S);
  lua_pushstring(L, S);
end;

function luaLowerCase(L : Plua_State) : Integer; cdecl;
var
  S: String;
begin
  Result:= 1;
  S:= lua_tostring(L, 1);
  S:= UTF8LowerCase(S);
  lua_pushstring(L, S);
end;

function luaConvertEncoding(L : Plua_State) : Integer; cdecl;
var
  S, FromEnc, ToEnc: String;
begin
  Result:= 1;
  S:= lua_tostring(L, 1);
  FromEnc:= lua_tostring(L, 2);
  ToEnc:= lua_tostring(L, 3);
  lua_pushstring(L, ConvertEncoding(S, FromEnc, ToEnc));
end;

function luaDetectEncoding(L : Plua_State) : Integer; cdecl;
var
  S: String;
begin
  Result:= 1;
  S:= lua_tostring(L, 1);
  lua_pushstring(L, DetectEncoding(S));
end;

function char_prepare(L : Plua_State; out Index: Integer): UnicodeString;
var
  Len: size_t;
  P: PAnsiChar;
begin
  P:= lua_tolstring(L, 1, @Len);
  Result:= UTF8ToUTF16(P, Len);
  if lua_isnumber(L, 2) then
    Index:= Integer(lua_tointeger(L, 2))
  else begin
    Index:= 1;
  end;
end;

function luaIsLower(L : Plua_State) : Integer; cdecl;
var
  Index: Integer;
  S: UnicodeString;
begin
  S:= char_prepare(L, Index);
  try
    lua_pushboolean(L, TCharacter.IsLower(S, Index));
    Result:= 1;
  except
    Result:= 0;
  end;
end;

function luaIsUpper(L : Plua_State) : Integer; cdecl;
var
  Index: Integer;
  S: UnicodeString;
begin
  S:= char_prepare(L, Index);
  try
    lua_pushboolean(L, TCharacter.IsUpper(S, Index));
    Result:= 1;
  except
    Result:= 0;
  end;
end;

function luaIsDigit(L : Plua_State) : Integer; cdecl;
var
  Index: Integer;
  S: UnicodeString;
begin
  S:= char_prepare(L, Index);
  try
    lua_pushboolean(L, TCharacter.IsDigit(S, Index));
    Result:= 1;
  except
    Result:= 0;
  end;
end;

function luaIsLetter(L : Plua_State) : Integer; cdecl;
var
  Index: Integer;
  S: UnicodeString;
begin
  S:= char_prepare(L, Index);
  try
    lua_pushboolean(L, TCharacter.IsLetter(S, Index));
    Result:= 1;
  except
    Result:= 0;
  end;
end;

function luaIsLetterOrDigit(L : Plua_State) : Integer; cdecl;
var
  Index: Integer;
  S: UnicodeString;
begin
  S:= char_prepare(L, Index);
  try
    lua_pushboolean(L, TCharacter.IsLetterOrDigit(S, Index));
    Result:= 1;
  except
    Result:= 0;
  end;
end;

function luaGetUnicodeCategory(L : Plua_State) : Integer; cdecl;
var
  Index: Integer;
  S: UnicodeString;
begin
  S:= char_prepare(L, Index);
  try
    lua_pushinteger(L, lua_Integer(TCharacter.GetUnicodeCategory(S, Index)));
    Result:= 1;
  except
    Result:= 0;
  end;
end;

function luaClipbrdClear(L : Plua_State) : Integer; cdecl;
begin
  Result:= 0;
  Clipboard.Clear;
end;

function luaClipbrdGetText(L : Plua_State) : Integer; cdecl;
begin
  Result:= 1;
  lua_pushstring(L, Clipboard.AsText);
end;

function luaClipbrdSetText(L : Plua_State) : Integer; cdecl;
begin
  Result:= 0;
  ClipboardSetText(luaL_checkstring(L, 1));
end;

function luaClipbrdSetHtml(L : Plua_State) : Integer; cdecl;
begin
  Result:= 0;
  Clipboard.SetAsHtml(luaL_checkstring(L, 1));
end;

function luaMessageBox(L : Plua_State) : Integer; cdecl;
var
  flags: Integer;
  text, caption: PAnsiChar;
begin
  Result:= 1;
  text:= luaL_checkstring(L, 1);
  caption:= luaL_checkstring(L, 2);
  flags:= Integer(lua_tointeger(L, 3));
  flags:= ShowMessageBox(text, caption, flags);
  lua_pushinteger(L, flags);
end;

function luaInputQuery(L : Plua_State) : Integer; cdecl;
var
  AValue: String;
  AMaskInput: Boolean;
  APrompt, ACaption: PAnsiChar;
begin
  Result:= 1;
  ACaption:= luaL_checkstring(L, 1);
  APrompt:= luaL_checkstring(L, 2);
  AMaskInput:= lua_toboolean(L, 3);
  AValue:= luaL_checkstring(L, 4);
  AMaskInput:= ShowInputQuery(ACaption, APrompt, AMaskInput, AValue);
  lua_pushboolean(L, AMaskInput);
  if AMaskInput then
  begin
    Result:= 2;
    lua_pushstring(L, AValue);
  end;
end;

function luaInputListBox(L : Plua_State) : Integer; cdecl;
var
  AValue: String = '';
  AIndex, ACount: Integer;
  AStringList: TStringList;
  APrompt, ACaption: PAnsiChar;
begin
  Result:= 1;
  if (lua_gettop(L) < 3) or (not lua_istable(L, 3)) then
  begin
    lua_pushnil(L);
    Exit;
  end;
  ACaption:= lua_tocstring(L, 1);
  APrompt:= lua_tocstring(L, 2);
  ACount:= lua_objlen(L, 3);
  AStringList:= TStringList.Create;
  for AIndex := 1 to ACount do
  begin
    lua_rawgeti(L, 3, AIndex);
    AStringList.Add(luaL_checkstring(L, -1));
    lua_pop(L, 1);
  end;
  if lua_isstring(L, 4) then begin
    AValue:= lua_tostring(L, 4);
  end;
  if ShowInputListBox(ACaption, APrompt, AStringList, AValue, AIndex) then
  begin
    Result:= 2;
    lua_pushstring(L, AValue);
    lua_pushinteger(L, AIndex + 1);
  end
  else begin
    lua_pushnil(L);
  end;
  AStringList.Free;
end;

function luaUTF8EscapeControlChars(L : Plua_State) : Integer; cdecl;
var
  S: String;
  Mode: Integer;
begin
  Result:= 1;
  S:= lua_tostring(L, 1);
  Mode:= lua_tointeger(L, 2);
  lua_pushstring(L, Utf8EscapeControlChars(S, TEscapeMode(Mode)));
end;

function luaDlgProc(pDlg: PtrUInt; DlgItemName: PAnsiChar; Msg, wParam, lParam: PtrInt): PtrInt; {$IFDEF MSWINDOWS}stdcall{$ELSE}cdecl{$ENDIF};
var
  L: Plua_State;
  Data: PLDLGUserData;
  Args: Integer;
begin
  Result:= 0;
  Data:= PLDLGUserData(SendDlgMsg(pDlg, nil, DM_GETDLGDATA, 0, 0));
  if not Assigned(Data) then
  begin
    logWrite('Data not assigned Msg = 0x' + IntToHex(Msg), lmtError, True, False);
    Exit;
  end;

  L:= Data^.L;
  lua_getglobal(L, Data^.FuncName);

  if not lua_isfunction(L, -1) then
  begin
    logWrite(Data^.FuncName + ' not a function', lmtError, True, False);
    Exit;
  end;
  Args:= 5;
  lua_pushinteger(L, pDlg);
  if (DlgItemName = nil) then
    lua_pushnil(L)
  else
    lua_pushstring(L, DlgItemName);
  lua_pushinteger(L, Msg);
  lua_pushinteger(L, wParam);
  lua_pushinteger(L, lParam);
  if (Data^.DataRef <> 0) then
  begin
    lua_rawgeti(L, LUA_REGISTRYINDEX,Data^.DataRef);
    Args:= Args + 1;
  end;
  LuaPCall(L, Args, 1);
  if not lua_isnil(L, -1) and (lua_type(L, -1) = LUA_TNUMBER) then
    Result := PtrInt(lua_tointeger(L, -1));
  lua_pop(L, 1);
  if (Msg = DN_CLOSE) then
  begin
    if (Data^.FuncRef > 0) then
    begin
      lual_unref(L, LUA_REGISTRYINDEX, Data^.FuncRef);
    end;
    if (Data^.DataRef > 0) then
    begin
      lual_unref(L, LUA_REGISTRYINDEX, Data^.DataRef);
    end;
  end;
end;

function luaDlgParamsToKeyStr(L : Plua_State) : Integer; cdecl;
var
  wParam, lParam: PtrInt;
  Key: ^Word absolute wParam;
begin
  Result:= 1;
  wParam:= PtrInt(lua_tointeger(L, 1));
  lParam:= PtrInt(lua_tointeger(L, 2));
  lua_pushstring(L, PAnsiChar(KeyAndShiftStateToKeyString(Key^, TShiftState(Integer(lParam)))));
end;

function luaDlgParamKeyHandled(L : Plua_State) : Integer; cdecl;
var
  Param: PtrInt;
  Key: ^Word absolute Param;
begin
  Result:= 0;
  Param:= PtrInt(lua_tointeger(L, 1));
  Key^:=0;
end;

function luaDlgParamToKeyCode(L : Plua_State) : Integer; cdecl;
var
  Param: PtrInt;
  Key: ^Word absolute Param;
begin
  Result:= 1;
  Param:= PtrInt(lua_tointeger(L, 1));
  lua_pushinteger(L, Key^);
end;

function luaDlgParamToStr(L : Plua_State) : Integer; cdecl;
var
  Param: PtrInt;
begin
  Result:= 1;
  Param:= PtrInt(lua_tointeger(L, 1));
  lua_pushstring(L, PAnsiChar(Param));
end;

function luaSendDlgMsg(L : Plua_State) : Integer; cdecl;
var
  pDlg: PtrUInt;
  DlgItemName: PAnsiChar;
  Bounds: TRect;
  Text: String;
  pRet, Msg, wParam, lParam: PtrInt;
  wType, lType, Index, Args: Integer;
  I, Count: Longint;
  Data: PLDLGUserData;
begin
  Result:= 1;
  Args:= lua_gettop(L);
  if (Args < 3) then
  begin
    logWrite('SendDlgMsg: missing arguments', lmtError, True, False);
    lua_pushnil(L);
    Exit;
  end;
  DlgItemName:= nil;
  pDlg := PtrUInt(Integer(lua_tointeger(L, 1)));
  if (lua_type(L, 2) = LUA_TSTRING) then
    DlgItemName:= lua_tocstring(L, 2);
  Msg:= PtrInt(lua_tointeger(L, 3));
  wParam:= 0;
  lParam:= 0;

  if (Msg = DM_GETDLGDATA) then
  begin
    Data:= PLDLGUserData(SendDlgMsg(pDlg, DlgItemName, DM_GETDLGDATA, 0, 0));
    lua_rawgeti(L, LUA_REGISTRYINDEX, Data^.DataRef);
    Exit;
  end
  else if (Msg = DM_SETDLGDATA) then
  begin
    Data:= PLDLGUserData(SendDlgMsg(pDlg, DlgItemName, DM_GETDLGDATA, 0, 0));
    for I:= 1 to 3 do
      lua_remove(L, 1);
    Index:= Data^.DataRef;
    Data^.DataRef:= luaL_ref(L, LUA_REGISTRYINDEX);
    lua_rawgeti(L, LUA_REGISTRYINDEX, Index);
    lual_unref(L, LUA_REGISTRYINDEX, Integer(Index));
    Exit;
  end;

  wType:= lua_type(L, 4);
  lType:= lua_type(L, 5);
  if (Args > 3) then
  begin
    if (wType = LUA_TSTRING) then
      wParam := PtrInt(lua_tocstring(L, 4))
    else if (wType = LUA_TBOOLEAN) then
    begin
      if (Boolean(lua_toboolean(L, 4)) = True) then
        wParam := 1;
    end
    else if (wType = LUA_TTABLE) and ((Msg = DM_SETDLGBOUNDS) or (Msg = DM_SETITEMBOUNDS))then
    begin
      lua_getfield(L, 4, 'Left');
      if (lua_type(L, -1) = LUA_TNUMBER) then
        Bounds.Left:= Longint(lua_tointeger(L, -1));
      lua_getfield(L, 4, 'Right');
      if (lua_type(L, -1) = LUA_TNUMBER) then
        Bounds.Right:= Longint(lua_tointeger(L, -1));
      lua_getfield(L, 4, 'Top');
      if (lua_type(L, -1) = LUA_TNUMBER) then
        Bounds.Top:= Longint(lua_tointeger(L, -1));
      lua_getfield(L, 4, 'Bottom');
      if (lua_type(L, -1) = LUA_TNUMBER) then
        Bounds.Bottom:= Longint(lua_tointeger(L, -1));
      wParam:= PtrInt(@Bounds);
    end
    else if (wType = LUA_TNUMBER) then
      wParam := PtrInt(lua_tointeger(L, 4));
  end;

  if (Msg = DM_LISTDELETE) then
  begin
    pRet:= SendDlgMsg(pDlg, DlgItemName, DM_LISTGETDATA, wParam, lParam);
    if (pRet > 0) then
    begin
      lual_unref(L, LUA_REGISTRYINDEX, Integer(pRet));
    end;
  end
  else if (Msg = DM_LISTCLEAR) then
  begin
    Count:= longint(SendDlgMsg(pDlg, DlgItemName, DM_LISTGETCOUNT, wParam, lParam));
    for I:= 0 to Count-1 do
    begin
      pRet:= SendDlgMsg(pDlg, DlgItemName, DM_LISTGETDATA, I, lParam);
      if (pRet > 0) then
      begin
        lual_unref(L, LUA_REGISTRYINDEX, Integer(pRet));
      end;
    end;
  end;

  if (Args = 5) then
  begin
    if (Msg = DM_LISTADD) or (Msg = DM_LISTSETDATA)then
    begin
      for Index:= 1 to 4 do
        lua_remove(L, 1);
      lParam:= PtrInt(luaL_ref(L, LUA_REGISTRYINDEX));
    end
    else if (lType = LUA_TSTRING) then
      lParam := PtrInt(lua_tocstring(L, 5))
    else if (lType = LUA_TBOOLEAN) then
    begin
      if (Boolean(lua_toboolean(L, 5)) = True) then
        lParam := 1;
    end
    else if (lType = LUA_TNUMBER) then
      lParam := PtrInt(lua_tointeger(L, 5));
  end;

  pRet:= SendDlgMsg(pDlg, DlgItemName, Msg, wParam, lParam);
  if (Msg = DM_ENABLE) or (Msg = DM_GETDROPPEDDOWN) or (Msg = DM_SHOWITEM) then
  begin
    lua_pushboolean(L, Boolean(pRet));
  end
  else if (Msg = DM_GETTEXT) or (Msg = DM_LISTGETITEM) then
  begin
    Text:= PAnsiChar(pRet);
    lua_pushstring(L, Text);
  end
  else if (Msg = DM_GETITEMBOUNDS) or (Msg = DM_GETDLGBOUNDS) then
  begin
    Bounds:= PRect(pRet)^;
    lua_newtable(L);
    lua_pushinteger(L, Bounds.Left);
    lua_setfield(L, -2, 'Left');
    lua_pushinteger(L, Bounds.Right);
    lua_setfield(L, -2, 'Right');
    lua_pushinteger(L, Bounds.Top);
    lua_setfield(L, -2, 'Top');
    lua_pushinteger(L, Bounds.Bottom);
    lua_setfield(L, -2, 'Bottom');
  end
  else if (Msg = DM_LISTGETDATA) then
  begin
    lua_rawgeti(L, LUA_REGISTRYINDEX, Integer(pRet));
  end
  else
  begin
    lua_pushinteger(L, pRet);
  end;
end;

function luaDialogBoxLFM(L : Plua_State) : Integer; cdecl;
var
  pRet: PtrUInt;
  sData: String;
  iFlags: UInt32;
  pReserved: Pointer;
  UserData: DLGUserData;
  Args: Integer;
begin
  Result:= 1;
  Args:= lua_gettop(L);
  if (Args < 3) or (not lua_isstring(L, 1) or not lua_isstring(L, 3)) then
  begin
    logWrite('SendDlgMsg: missing or insufficient arguments', lmtError, True, False);
    lua_pushnil(L);
    Exit;
  end;
  UserData.L:= L;
  UserData.FuncRef:= 0;
  UserData.DataRef:= 0;
  UserData.FuncName:= '';
  sData:= lua_tostring(L, 1);
  lua_remove(L, 1);
  iFlags:= DB_FILENAME;
  if lua_isboolean(L, 1) and (not lua_toboolean(L, 1)) then
    iFlags:= DB_LFM;
  lua_remove(L, 1);
  UserData.FuncName:= lua_tocstring(L, 1);
  if (Args = 4) then
  begin
    lua_remove(L, 1);
    UserData.DataRef:= luaL_ref(L, LUA_REGISTRYINDEX);
  end;
  pRet:= DialogBoxParam(PAnsiChar(sData), LongWord(Length(sData)), @luaDlgProc, iFlags, @UserData, pReserved);
  lua_pushinteger(L, Integer(pRet));
end;
function luaExtractFromArchive(L: Plua_State) : Integer; cdecl;
var
  AIndex, ACount: Integer;
  AStringList: TStringList;
  ArcName, DestDir: String;
  Sign: Boolean;
begin
  Result:= 1;
  if (lua_gettop(L) <> 4) or (not lua_istable(L, 3)) then
  begin
    lua_pushboolean(L, False);
    Exit;
  end;
  ArcName:= lua_tostring(L, 1);
  Sign:= Boolean(lua_toboolean(L, 2));
  ACount:= lua_objlen(L, 3);
  AStringList:= TStringList.Create;
  AStringList.Sorted:= True;
  for AIndex := 1 to ACount do
  begin
    lua_rawgeti(L, 3, AIndex);
    AStringList.Add(luaL_checkstring(L, -1));
    lua_pop(L, 1);
  end;
  DestDir:= lua_tostring(L, 4);
  lua_pushboolean(L, ExtractFromArchive(ArcName, Sign, AStringList, DestDir));
  AStringList.Free;
end;

function luaConfigGetContent(L : Plua_State) : Integer; cdecl;
var
  Path, Content: String;
  Node: TXmlNode;
begin
  Result:= 1;
  Path:= lua_tostring(L, 1);
  Node:= gConfig.FindNode(gConfig.RootNode, Path, False);
  if (Node = nil) then
  begin
    lua_pushnil(L);
    Exit;
  end;
  Content:= gConfig.GetContent(Node);
  lua_pushstring(L, Content);
end;

function luaLogWrite(L : Plua_State) : Integer; cdecl;
var
  sText: String;
  bForce: Boolean = True;
  bLogFile: Boolean = False;
  LogMsgType: TLogMsgType = lmtInfo;
begin
  Result:= 0;
  sText:= lua_tostring(L, 1);
  if lua_isnumber(L, 2) then
    LogMsgType:= TLogMsgType(lua_tointeger(L, 2));
  if lua_isboolean(L, 3) then
    bForce:= lua_toboolean(L, 3);
  if lua_isboolean(L, 4) then
    bLogFile:= lua_toboolean(L, 4);
  logWrite(sText, LogMsgType, bForce, bLogFile);
end;

function luaExecuteCommand(L : Plua_State) : Integer; cdecl;
var
  Index,
  Count: Integer;
  Command: String;
  Args: array of String;
  Res: TCommandFuncResult;
begin
  Result:= 1;
  Res:= cfrNotFound;
  Count:= lua_gettop(L);
  if Count > 0 then
  begin
    // Get command
    Command:= lua_tostring(L, 1);
    // Get parameters
    SetLength(Args, Count - 1);
    for Index:= 2 to Count do
     Args[Index - 2]:= lua_tostring(L, Index);
    // Execute internal command
    Res:= frmMain.Commands.Commands.ExecuteCommand(Command, Args);
    Application.ProcessMessages;
  end;
  lua_pushboolean(L, Res = cfrSuccess);
end;

function luaCurrentPanel(L : Plua_State) : Integer; cdecl;
var
  Count: Integer;
begin
  Result:= 1;
  Count:= lua_gettop(L);
  lua_pushinteger(L, Integer(frmMain.SelectedPanel));
  if (Count > 0) then
    frmMain.SetActiveFrame(TFilePanelSelect(lua_tointeger(L, 1)));
end;

procedure luaP_register(L : Plua_State; n : PChar; f : lua_CFunction);
begin
  lua_pushcfunction(L, f);
  lua_setfield(L, -2, n);
end;

procedure luaC_register(L : Plua_State; n : PChar; c : PChar);
begin
  lua_pushstring(L, c);
  lua_setfield(L, -2, n);
end;

procedure RegisterPackages(L: Plua_State);
begin
  lua_newtable(L);
    luaP_register(L, 'Sleep', @luaSleep);
    luaP_register(L, 'FindNext', @luaFindNext);
    luaP_register(L, 'FindFirst', @luaFindFirst);
    luaP_register(L, 'FindClose', @luaFindClose);
    luaP_register(L, 'FileExists', @luaFileExists);
    luaP_register(L, 'FileGetAttr', @luaFileGetAttr);
    luaP_register(L, 'GetTickCount', @luaGetTickCount);
    luaP_register(L, 'DirectoryExists', @luaDirectoryExists);
    luaP_register(L, 'CreateDirectory', @luaCreateDirectory);

    luaP_register(L, 'CreateHardLink', @luaCreateHardLink);
    luaP_register(L, 'CreateSymbolicLink', @luaCreateSymbolicLink);
    luaP_register(L, 'ReadSymbolicLink', @luaReadSymbolicLink);

    luaP_register(L, 'ExtractFileExt', @luaExtractFileExt);
    luaP_register(L, 'ExtractFileDir', @luaExtractFileDir);
    luaP_register(L, 'ExtractFilePath', @luaExtractFilePath);
    luaP_register(L, 'ExtractFileName', @luaExtractFileName);
    luaP_register(L, 'ExtractFileDrive', @luaExtractFileDrive);

    luaP_register(L, 'GetAbsolutePath', @luaGetAbsolutePath);
    luaP_register(L, 'GetRelativePath', @luaGetRelativePath);

    luaP_register(L, 'MatchesMask', @luaMatchesMask);
    luaP_register(L, 'MatchesMaskList', @luaMatchesMaskList);

    luaP_register(L, 'GetTempName', @luaGetTempName);

    luaC_register(L, 'PathDelim', PathDelim);
  lua_setglobal(L, 'SysUtils');

  lua_newtable(L);
    luaP_register(L, 'Pos', @luaPos);
    luaP_register(L, 'Next', @luaNext);
    luaP_register(L, 'Copy', @luaCopy);
    luaP_register(L, 'Length', @luaLength);
    luaP_register(L, 'UpperCase', @luaUpperCase);
    luaP_register(L, 'LowerCase', @luaLowerCase);
    luaP_register(L, 'ConvertEncoding', @luaConvertEncoding);
    luaP_register(L, 'DetectEncoding', @luaDetectEncoding);
    luaP_register(L, 'EscapeControlChars', @luaUTF8EscapeControlChars);
  lua_setglobal(L, 'LazUtf8');

  lua_newtable(L);
    luaP_register(L, 'IsLower', @luaIsLower);
    luaP_register(L, 'IsUpper', @luaIsUpper);
    luaP_register(L, 'IsDigit', @luaIsDigit);
    luaP_register(L, 'IsLetter', @luaIsLetter);
    luaP_register(L, 'IsLetterOrDigit', @luaIsLetterOrDigit);
    luaP_register(L, 'GetUnicodeCategory', @luaGetUnicodeCategory);
  lua_setglobal(L, 'Char');

  lua_newtable(L);
    luaP_register(L, 'Clear', @luaClipbrdClear);
    luaP_register(L, 'GetAsText', @luaClipbrdGetText);
    luaP_register(L, 'SetAsText', @luaClipbrdSetText);
    luaP_register(L, 'SetAsHtml', @luaClipbrdSetHtml);
  lua_setglobal(L, 'Clipbrd');

  lua_newtable(L);
    luaP_register(L, 'MessageBox', @luaMessageBox);
    luaP_register(L, 'InputQuery', @luaInputQuery);
    luaP_register(L, 'InputListBox', @luaInputListBox);

    luaP_register(L, 'ParamKeyHandled', @luaDlgParamKeyHandled);
    luaP_register(L, 'ParamsToKeyStr', @luaDlgParamsToKeyStr);
    luaP_register(L, 'ParamToKeyCode', @luaDlgParamToKeyCode);
    luaP_register(L, 'ParamToStr', @luaDlgParamToStr);
    luaP_register(L, 'SendDlgMsg', @luaSendDlgMsg);
    luaP_register(L, 'DialogBoxLFM', @luaDialogBoxLFM);
    lua_setglobal(L, 'Dialogs');

  lua_newtable(L);
    luaP_register(L, 'LogWrite', @luaLogWrite);
    luaP_register(L, 'CurrentPanel', @luaCurrentPanel);
    luaP_register(L, 'ExecuteCommand', @luaExecuteCommand);
    luaP_register(L, 'ConfigGetContent', @luaConfigGetContent);
    luaP_register(L, 'ExtractFromArchive', @luaExtractFromArchive);
    lua_pushinteger(L, VERSION_API);
    lua_setfield(L, -2, 'VersionAPI');
  lua_setglobal(L, 'DC');

  ReplaceLibrary(L);
end;

procedure SetPackagePath(L: Plua_State; const Path: String);
var
  APath, ANewPath: String;
const
  COMMON_FILE = 'scripts' + PathDelim + 'doublecmd-common.lua;';
begin
  lua_getglobal(L, 'package');
    // Set package.path
    lua_getfield(L, -1, 'path');
      APath := lua_tostring(L, -1);
      ANewPath := gpExePath + COMMON_FILE + Path;
      APath := StringReplace(APath, '.' + PathDelim, ANewPath, []);
    lua_pop(L, 1);
    lua_pushstring(L, APath);
    lua_setfield(L, -2, 'path');
    // Set package.cpath
    lua_getfield(L, -1, 'cpath');
      APath := lua_tostring(L, -1);
      APath := StringReplace(APath, '.' + PathDelim, Path, []);
    lua_pop(L, 1);
    lua_pushstring(L, APath);
    lua_setfield(L, -2, 'cpath');
  lua_pop(L, 1);
end;

function LuaPCall(L: Plua_State; nargs, nresults: Integer): Boolean;
var
  Status: Integer;
begin
  Status:= lua_pcall(L, nargs, nresults, 0);
  // Check execution result
  if Status <> 0 then begin
    logWrite(lua_tostring(L, -1), lmtError, True, False);
  end;
  Result:= (Status = 0);
end;

function ExecuteScript(const FileName: String; Args: array of String; var sErrorToReportIfAny:string): Boolean;
var
  L: Plua_State;
  Index: Integer;
  Count: Integer;
  Script: String;
  Status: Integer;
begin
  Result:= False;
  sErrorToReportIfAny := '';

  // Load Lua library
  if not IsLuaLibLoaded then
  begin
    if not LoadLuaLib(mbExpandFileName(gLuaLib)) then
    begin
      sErrorToReportIfAny := Format(rsMsgScriptCantFindLibrary, [gLuaLib]);
      Exit;
    end;
  end;

  // Get script file name
  Script:= mbFileNameToSysEnc(FileName);

  L := lua_open;
  if Assigned(L) then
  begin
    luaL_openlibs(L);
    RegisterPackages(L);
    SetPackagePath(L, ExtractFilePath(Script));

    // Load script from file
    Status := luaL_loadfile(L, PAnsiChar(Script));
    if (Status = 0) then
    begin
      // Push arguments
      Count:= Length(Args);
      if (Count > 0) then
      begin
        for Index := 0 to Count - 1 do begin
          lua_pushstring(L, Args[Index]);
        end;
      end;
      // Execute script
      Status := lua_pcall(L, Count, 0, 0)
    end;

    // Check execution result
    if Status <> 0 then begin
      Script:= CeRawToUtf8(lua_tostring(L, -1));
      UTF8FixBroken(PChar(Script));
      MessageDlg(Script, mtError, [mbOK], 0);
    end;

    lua_close(L);

    Result:= (Status = 0);
  end;
end;

function ExecuteScriptString(Script: PAnsiChar): Boolean;
var
  L: Plua_State;
  Status: Integer;
begin
  Result:= False;

  // Load Lua library
  if not IsLuaLibLoaded and not LoadLuaLib(mbExpandFileName(gLuaLib)) then
    Exit;

  L := lua_open;
  if Assigned(L) then
  begin
    luaL_openlibs(L);
    RegisterPackages(L);

    // Load script from file
    Status := luaL_dostring(L, Script);

    // Check execution result
    if Status <> 0 then begin
      logWrite(CeRawToUtf8(lua_tostring(L, -1)), lmtError, True, False);
    end;

    lua_close(L);

    Result:= (Status = 0);
  end;
end;

end.

