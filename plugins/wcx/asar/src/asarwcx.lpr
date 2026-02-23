library AsarWcx;

{$include calling.inc}

uses
{$IFDEF UNIX}
  cthreads, BaseUnix,
{$ENDIF}
  FPCAdds, SysUtils, StrUtils, Classes, uAsar, DCConvertEncoding, WcxPlugin;

threadvar
  gProcessDataProcW : TProcessDataProcW;

type
  TRecord = class
    Asar: TAsarArchive;
    Index: Integer;
    Count: Integer;
  end;

function OpenArchiveW(var ArchiveData : tOpenArchiveDataW) : TArcHandle; dcpcall;
var
  AFileName: String;
  AHandle: TRecord absolute Result;
begin
  AHandle:= TRecord.Create;
  AFileName:= CeUtf16ToUtf8(UnicodeString(ArchiveData.ArcName));
  try
    AHandle.Index:= 0;
    AHandle.Asar:= TAsarArchive.Create;
    if not AHandle.Asar.Open(AFileName) then
      raise Exception.Create(EmptyStr);
    AHandle.Count:= AHandle.Asar.Count;
    Result:= TArcHandle(AHandle);
  except
    ArchiveData.OpenResult:= E_EOPEN;
    if Assigned(AHandle) then
    begin
      AHandle.Asar.Close;
      AHandle.Asar.Free;
      FreeAndNil(AHandle);
    end;
  end;
end;

function ReadHeaderExW(hArcData : TArcHandle; var HeaderData: THeaderDataExW) : Integer; dcpcall;
var
  AFile: TAsarItem;
  AHandle: TRecord absolute hArcData;
begin
  if AHandle.Index >= AHandle.Count then Exit(E_END_ARCHIVE);

  AFile:= AHandle.Asar.Items[AHandle.Index];
  HeaderData.FileName:= CeUtf8ToUtf16(AFile.FileName);
  HeaderData.UnpSize:= Int64Rec(AFile.Size).Lo;
  HeaderData.UnpSizeHigh:= Int64Rec(AFile.Size).Hi;
  HeaderData.PackSize:= HeaderData.UnpSize;
  HeaderData.PackSizeHigh:= HeaderData.UnpSizeHigh;
{$IF DEFINED(UNIX)}
  if AFile.IsDir then
    HeaderData.FileAttr:= S_IFDIR
  else if AFile.IsLink then
    HeaderData.FileAttr:= S_IFLNK
  else
    HeaderData.FileAttr:= S_IFREG;

  if AFile.IsExecutable or AFile.IsDir then
    HeaderData.FileAttr:= HeaderData.FileAttr or &0755
  else
    HeaderData.FileAttr:= HeaderData.FileAttr or &0644;
{$ELSEIF DEFINED(MSWINDOWS)}
  if AFile.IsDir then
    HeaderData.FileAttr:= HeaderData.FileAttr or faDirectory
  else if AFile.IsLink then
    HeaderData.FileAttr:= HeaderData.FileAttr or faSymLink;
{$ENDIF}
  Result:= E_SUCCESS;
end;

function ProcessFileW(hArcData : TArcHandle; Operation : Integer; DestPath, DestName : PWideChar) : Integer; dcpcall;
var
  AFile: TAsarItem;
  DestNameUtf8: String;
  AHandle: TRecord absolute hArcData;
  Dst: TFileStream;
begin
  Result:= E_SUCCESS;
  DestNameUtf8:= CeUtf16ToUtf8(UnicodeString(DestName));
  AFile:= AHandle.Asar.Items[AHandle.Index];
  AHandle.Asar.ProcFile:= AFile.FileName;

  case Operation of
  PK_TEST:
    begin
      Result:= AHandle.Asar.VerifyItem(AHandle.Index);
    end;

  PK_EXTRACT:
    begin
      try
        if AFile.IsLink then
        begin
          Result:= E_NOT_SUPPORTED;
        end
        else
        begin
          Dst:= TFileStream.Create(DestNameUtf8, fmCreate);
          Result:= AHandle.Asar.ExtractItem(AHandle.Index, Dst);
          Dst.Free;
{
{$IFDEF UNIX}
          if AFile.IsExecutable then
            fpChmod(DestNameUtf8, &0755);
{$ENDIF}
}
        end;
      except
        Result:= E_EWRITE;
      end;
    end;

  PK_SKIP:
    begin

    end;
  end;
  Inc(AHandle.Index);
end;

function CloseArchive (hArcData : TArcHandle) : Integer; dcpcall;
var
  AHandle: TRecord absolute hArcData;
begin
  if hArcData <> wcxInvalidHandle then
  begin
    AHandle.Asar.Close;
    AHandle.Asar.Free;
    FreeAndNil(AHandle);
  end;
  Result:= E_SUCCESS;
end;

procedure SetProcessDataProcW (hArcData : TArcHandle; pProcessDataProc : TProcessDataProcW); dcpcall;
var
  AHandle: TRecord absolute hArcData;
begin
  if (hArcData <> wcxInvalidHandle) then
    AHandle.Asar.ProcessDataProc:= pProcessDataProc
  else begin
    gProcessDataProcW:= pProcessDataProc;
  end;
end;

procedure SetChangeVolProcW(hArcData: TArcHandle; pChangeVolProc: TChangeVolProcW); dcpcall; export;
begin

end;

function PackFilesW(PackedFile: PWideChar; SubPath: PWideChar; SrcPath: PWideChar; AddList: PWideChar; Flags: Integer): Integer; dcpcall; export;
const
  UnpackedExts: array of string = ('.node', '.dll', '.exe', '.so', '.dylib');

var
  AFileName: string;
  Asar: TAsarArchive;
  FileName: UnicodeString;
  SubPathU, SrcPathU: string;
  Unpacked: Boolean = False;
  Executable: Boolean = False;
  DiskPath, AsarPath, Ext: string;
begin
  if (Flags and PK_PACK_MOVE_FILES) <> 0 then
    Exit(E_NOT_SUPPORTED);

  Result:= E_SUCCESS;
  AFileName:= CeUtf16ToUtf8(UnicodeString(PackedFile));
  SubPathU:= CeUtf16ToUtf8(UnicodeString(SubPath));
  SrcPathU:= CeUtf16ToUtf8(UnicodeString(SrcPath));
  Asar:= TAsarArchive.Create;
  try
    Asar.Open(AFileName);
    Asar.ProcessDataProc:= gProcessDataProcW;

    while AddList^ <> #0 do
    begin
      FileName:= UnicodeString(AddList);
      AsarPath:= SubPathU + CeUtf16ToUtf8(FileName);
      AsarPath:= StringReplace(AsarPath, PathDelim, '/', [rfReplaceAll]);

      // If ends with '/' then add directory
      if FileName[Length(FileName)] = PathDelim then
      begin
        //Asar.AddDir(AsarPath);
      end
      else
      begin
        DiskPath:= SrcPathU + CeUtf16ToUtf8(FileName);
{
        Ext:= ExtractFileExt(DiskPath);
        Unpacked:= MatchStr(Ext.ToLower, UnpackedExts);
{$IFDEF UNIX}
        Executable:= (fpAccess(DiskPath, X_OK) = 0);
{$ENDIF}
        Asar.AddFile(DiskPath, AsarPath, Unpacked, Executable);
}
        Asar.AddFile(DiskPath, AsarPath);
      end;

      Inc(AddList, Length(FileName) + 1);
    end;
  finally
    Result:= Asar.SaveTo(AFileName);
    Asar.Close;
    Asar.Free;
  end;
end;

function DeleteFilesW(PackedFile, DeleteList : PWideChar) : Integer;dcpcall; export;
var
  AFileName: string;
  Asar: TAsarArchive;
  FileName: UnicodeString;
begin
  Result:= E_SUCCESS;
  AFileName:= CeUtf16ToUtf8(UnicodeString(PackedFile));
  Asar:= TAsarArchive.Create;
  try
    if not Asar.Open(AFileName) then
    begin
      Result:= E_EOPEN;
      raise Exception.Create(EmptyStr);
    end;
    Asar.ProcessDataProc:= gProcessDataProcW;

    while DeleteList^ <> #0 do
    begin
      FileName:= UnicodeString(DeleteList);
      Asar.RemoveFile(CeUtf16ToUtf8(FileName));
      Inc(DeleteList, Length(FileName) + 1);
    end;

  finally
    Result:= Asar.SaveTo(AFileName);
    Asar.Close;
    Asar.Free;
  end;
end;

function GetPackerCaps : Integer;dcpcall;
begin
  //Result:= PK_CAPS_NEW or PK_CAPS_DELETE or PK_CAPS_MODIFY or PK_CAPS_MULTIPLE;
  Result:= PK_CAPS_MULTIPLE;
end;

function GetBackgroundFlags: Integer; dcpcall; export;
begin
  //Result:= BACKGROUND_UNPACK or BACKGROUND_PACK;
  Result:= BACKGROUND_UNPACK;
end;

exports
  { Mandatory }
  OpenArchiveW,
  ReadHeaderExW,
  ProcessFileW,
  CloseArchive,
  SetProcessDataProcW,
  SetChangeVolProcW,
  { Optional }
  PackFilesW,
  DeleteFilesW,
  GetPackerCaps,
  GetBackgroundFlags;

{$R *.res}

begin
end.

