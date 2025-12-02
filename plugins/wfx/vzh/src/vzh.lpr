library vzh;

{$mode objfpc}{$H+}
{$include calling.inc}

uses
  Classes,
  Extension,
  SysUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  WfxPlugin;

{$R *.res}

const
  BUFSIZE = 4096;
  BUFMSG = 'Read buffer size';
  COPYMSG = 'Write successful copy operations to the log?';
  QUIETMSG = 'Do not show read error dialogs?';

var
  gPluginNr: Integer;
  gProgressProc: tProgressProc;
  gLogProc: tLogProc;
  gRequestProc: tRequestProc;
  gBufferSize: Integer = BUFSIZE;
  gConnected: Boolean = False;
  gShowComplete: Boolean = False;
  gQuietCopy: Boolean = False;

function FsInit(PluginNr:Integer; pProgressProc:tProgressProc; pLogProc:tLogProc;
                pRequestProc:tRequestProc):Integer; dcpcall; export;
begin
  gPluginNr:= PluginNr;
  gProgressProc:= pProgressProc;
  gLogProc:= pLogProc;
  gRequestProc:= pRequestProc;
  Result:= 0;
end;

function FsFindFirst(Path :PChar;var FindData: tWIN32FINDDATA):thandle; dcpcall; export;
begin
  if not gConnected then
  begin
    gLogProc(gPluginNr, MSGTYPE_CONNECT, 'CONNECT ' + PathDelim);
    gConnected:= True;
  end;
  {$IFDEF MSWINDOWS}
  SetLastError(ERROR_NO_MORE_FILES);
  {$ENDIF}
  Result:= wfxInvalidHandle;
end;

function FsFindNext(Hdl: THandle; var FindData: tWIN32FINDDATA): BOOL; dcpcall; export;
begin
  Result:= False;
end;

function FsFindClose(Hdl: THandle):Integer; dcpcall; export;
begin
  Result:= 0;
end;

function FsPutFile(LocalName, RemoteName: PChar; CopyFlags: Integer): Integer; dcpcall; export;
var
  Pos: Double;
  Total, Read, FileSize: Int64;
  Buffer: Pointer;
  Stream: TFileStream;
begin
  Result:= FS_FILE_READERROR;
  try
    Stream:= TFileStream.Create(LocalName, fmOpenRead);
    Result:= FS_FILE_OK;
    Stream.Position:= 0;
    GetMem(Buffer, gBufferSize);
    Pos:= 0;
    Total:= 0;
    FileSize:= Stream.Size;
    while Total < FileSize do
    begin
      Read:= Stream.Read(Buffer^, gBufferSize);
      if Read = 0 then
      begin
        gLogProc(gPluginNr, MSGTYPE_IMPORTANTERROR, PChar(LocalName + ': read error at position ' + IntToStr(Total) + ' (' + FloatToStr(Pos) + '%)' ));
        Result:= FS_FILE_READERROR;
        break;
      end;
      Inc(Total, Read);
      Pos:= Total * 100 / FileSize;
      if gProgressProc(gPluginNr, LocalName, RemoteName, Trunc(Pos)) <> 0 then
      begin
        Result:= FS_FILE_USERABORT;
        break;
      end;
    end;
    Stream.Free;
    if Assigned(Buffer) then
      FreeMem(Buffer);
    if gShowComplete and (Result = FS_FILE_OK) then
      gLogProc(gPluginNr, MSGTYPE_TRANSFERCOMPLETE, PChar(LocalName + ' -> ' + RemoteName));
  except
    on E:Exception do
      gLogProc(gPluginNr, MSGTYPE_IMPORTANTERROR, PChar(E.Message));
  end;
  if gQuietCopy and (Result <> FS_FILE_USERABORT) then
    Result:= FS_FILE_OK;
end;

function FsMkDir(RemoteDir: PChar): BOOL; dcpcall;
begin
  Result:= True;
end;

function FsExecuteFile(MainWin: THandle; RemoteName, Verb: PChar): Integer; dcpcall; export;
var
  text: array[0..5] of char;
begin
  if (Verb = 'properties') and Assigned(gRequestProc) then
  begin
    text:= IntToStr(gBufferSize);
    if gRequestProc(gPluginNr, RT_Other, '', BUFMSG, text, 4) = True then
    begin
      gBufferSize:= StrToIntDef(text, BUFSIZE);
      gShowComplete:= gRequestProc(gPluginNr, RT_MsgYesNo, '', COPYMSG, nil, 0);
      gQuietCopy:= gRequestProc(gPluginNr, RT_MsgYesNo, '', QUIETMSG, nil, 0);
    end;
  end;
  Result:= FS_EXEC_OK;
end;

function FsDisconnect(DisconnectRoot: PChar): BOOL; dcpcall; export;
begin
  gLogProc(gPluginNr, MSGTYPE_DISCONNECT, 'DISCONNECT ' + PathDelim);
  gConnected:= False;
  Result:= True;
end;

function FsGetBackgroundFlags: Integer; dcpcall; export;
begin
  Result:= BG_UPLOAD;
end;

exports
  FsInit,
  FsFindFirst,
  FsFindNext,
  FsFindClose,
  FsPutFile,
  FsMkDir,
  FsExecuteFile,
  FsDisconnect,
  FsGetBackgroundFlags;
end.
