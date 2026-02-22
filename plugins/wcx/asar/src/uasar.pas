unit uAsar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, DCPsha256, Masks,
  DCOSUtils, DCConvertEncoding, WcxPlugin;

const
  BUF_SIZE = 4095;
  ASAR_BLOCK_SIZE = 4 * 1024 * 1024;

type
  TAsarItem = record
    FileName: string;
    Size: Int64;
    Offset: Int64;
    DiskPath: string;
    IsExternal: Boolean;
    IsExecutable: Boolean;
    IsDir: Boolean;
    SHA256: string;
  end;
  PAsarItem = ^TAsarItem;

  { TAsarArchive }

  TAsarArchive = class
  private
    FStream: TFileStream;
    FItems: TFPList;
    FDataStart: Int64;
    FProcFile: PWideChar;
    FProcessDataProc: TProcessDataProcW;

    procedure ClearItems;
    procedure ParseJson(Obj: TJSONObject; const ParentPath: string);
    procedure AddToTree(Root: TJSONObject; const VirtualPath: string; FileObj: TJSONObject);
    function Align4(Val: Int64): Int64;
    procedure WritePickle(Stream: TStream; JsonSize: Cardinal);
    procedure WritePadding(Stream: TStream);
    function DigestToHex(const D: array of byte): string;
    procedure CalculateIntegrity(const AFilePath: string; out FullHash: string; out Blocks: TJSONArray);
    function GetCount: Integer;
    procedure SetProc(ProcessDataProc: TProcessDataProcW);
    procedure SetProcFile(FileName: string);
    function GetItem(Index: Integer): TAsarItem;
    function WriteData(Src, Dst: TStream; Size: Int64): Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function Open(const FileName: string): Boolean;
    procedure Close;

    property ProcessDataProc: TProcessDataProcW write SetProc;
    property ProcFile: string write SetProcFile;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TAsarItem read GetItem; default;

    function ExtractItem(Index: Integer; Dest: TStream): Integer;
    function VerifyItem(Index: Integer): Integer;

    procedure AddFile(const DiskPath, AsarPath: string; External: Boolean = False; Executable: Boolean = False);
    procedure AddDir(const AsarPath: string);
    procedure RemoveFile(const AsarPathMask: string);
    function SaveTo(const FileName: string): Integer;
  end;

implementation

constructor TAsarArchive.Create;
begin
  FItems:= TFPList.Create;
  FProcFile:= '';
end;

destructor TAsarArchive.Destroy;
begin
  Close;
  ClearItems;
  FItems.Free;
  inherited;
end;

procedure TAsarArchive.ClearItems;
var
  I: Integer;
begin
  for I:= 0 to FItems.Count - 1 do
    Dispose(PAsarItem(FItems[I]));
  FItems.Clear;
end;

function TAsarArchive.Align4(Val: Int64): Int64;
begin
  Result:= (Val + 3) and not 3;
end;

function TAsarArchive.DigestToHex(const D: array of byte): string;
var
  I: Integer;
begin
  Result:= '';
  for I:= 0 to 31 do
    Result:= Result + IntToHex(D[I], 2).ToLower;
end;

function TAsarArchive.GetCount: Integer;
begin
  Result:= FItems.Count;
end;

procedure TAsarArchive.SetProc(ProcessDataProc: TProcessDataProcW);
begin
  FProcessDataProc:= ProcessDataProc;
end;

procedure TAsarArchive.SetProcFile(FileName: string);
begin
  FProcFile:= PWideChar(CeUtf8ToUtf16(FileName));
end;

function TAsarArchive.GetItem(Index: Integer): TAsarItem;
begin
  Result:= PAsarItem(FItems[Index])^;
end;

function TAsarArchive.WriteData(Src, Dst: TStream; Size: int64): Integer;
var
  Buf: array[0..BUF_SIZE] of Byte;
  Total: Int64 = 0;
  Read, Len: Integer;
begin
  Result:= E_SUCCESS;
  Len:= SizeOf(Buf);
  try
    while Total < Size do
    begin
      if (Size - Total < SizeOf(Buf)) then
        Len:= Size - Total;

      Read:= Src.Read(Buf, Len);

      if Assigned(FProcessDataProc) and (FProcessDataProc(FProcFile, Read) = 0) then
        Exit(E_EABORTED);
      if Read = 0 then
        break;

      Dst.Write(Buf, Read);
      Inc(Total, Read);
    end;
  except
    Exit(E_EWRITE);
  end;
end;

procedure TAsarArchive.CalculateIntegrity(const AFilePath: string; out FullHash: string; out Blocks: TJSONArray);
var
  Src: TFileStream;
  FullCtx, BlockCtx: TDCP_sha256;
  FullDigest, BlockDigest: array[0..31] of byte;
  Buf: array[0..BUF_SIZE] of Byte;
  Read, BytesInBlock: Integer;
begin
  Blocks:= TJSONArray.Create;
  Src:= TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyNone);
  try
    BytesInBlock:= 0;
    FullCtx:= TDCP_sha256.Create(nil);
    BlockCtx:= TDCP_sha256.Create(nil);
    FullCtx.Init;
    BlockCtx.Init;
    while Src.Position < Src.Size do begin
      Read:= Src.Read(Buf, SizeOf(Buf));
      FullCtx.Update(Buf, Read);
      BlockCtx.Update(Buf, Read);
      Inc(BytesInBlock, Read);
      if (BytesInBlock >= ASAR_BLOCK_SIZE) or (Src.Position = Src.Size) then
      begin
        BlockCtx.Final(BlockDigest);
        Blocks.Add(DigestToHex(BlockDigest));
        BytesInBlock:= 0;
        BlockCtx.Init;
      end;
    end;
    FullCtx.Final(FullDigest);
    FullHash:= DigestToHex(FullDigest);
  finally
    Src.Free;
  end;
end;

procedure TAsarArchive.ParseJson(Obj: TJSONObject; const ParentPath: string);
var
  I: Integer;
  FilesNode, Item, Intgr: TJSONObject;
  NewItem: PAsarItem;
  Name: string;
begin
  FilesNode:= Obj.Find('files') as TJSONObject;

  if not Assigned(FilesNode) then
    Exit;

  for I:= 0 to FilesNode.Count - 1 do
  begin
    Name:= FilesNode.Names[I];
    Item:= FilesNode.Items[I] as TJSONObject;
    if Assigned(Item.Find('files')) then
      ParseJson(Item, ParentPath + Name + '/')
    else
    begin
      New(NewItem);
      NewItem^.FileName:= ParentPath + Name;
      NewItem^.Size:= Item.Int64s['size'];

      if Item.Find('executable') <> nil then
        NewItem^.IsExecutable:= Item.Booleans['executable']
      else
        NewItem^.IsExecutable:= False;

      if Item.Find('unpacked') <> nil then
        NewItem^.IsExternal:= Item.Booleans['unpacked']
      else
        NewItem^.IsExternal:= False;

      if NewItem^.IsExternal then
        NewItem^.Offset:= -1
      else
        NewItem^.Offset:= StrToInt64(Item.Strings['offset']);

      Intgr:= Item.Find('integrity') as TJSONObject;
      if Assigned(Intgr) then
        NewItem^.SHA256:= Intgr.Strings['hash']
      else
        NewItem^.SHA256:= '';

      NewItem^.DiskPath:= '';

      FItems.Add(NewItem);
    end;
  end;
end;

function TAsarArchive.Open(const FileName: string): Boolean;
var
  H: array[0..3] of Cardinal;
  JsonStr: string;
  Root: TJSONObject;
begin
  Result:= False;
  try
    Close; ClearItems;
    FStream:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    FStream.ReadBuffer(H, 16);
    SetLength(JsonStr, H[3]);
    FStream.ReadBuffer(Pointer(JsonStr)^, H[3]);
    FDataStart:= Align4(16 + H[3]);
    Root:= GetJSON(JsonStr) as TJSONObject;
    try
      ParseJson(Root, '');
      Result:= True;
    finally
      Root.Free;
    end;
  except
    Result:= False;
  end;
end;

procedure TAsarArchive.Close;
begin
  if Assigned(FStream) then
    FreeAndNil(FStream);
end;

function TAsarArchive.ExtractItem(Index: Integer; Dest: TStream): Integer;
var
  Path: string;
  Item: PAsarItem;
  Extrnl: TFileStream;
begin
  Result:= E_SUCCESS;
  Item:= FItems[Index];
  if Item^.IsExternal then
  begin
    Path:= FStream.FileName + '.unpacked' + PathDelim + StringReplace(Item^.FileName, '/', PathDelim, [rfReplaceAll]);
    Extrnl:= TFileStream.Create(Path, fmOpenRead or fmShareDenyNone);
    WriteData(Extrnl, Dest, Extrnl.Size);
    Extrnl.Free;
  end
  else
  begin
    FStream.Position:= FDataStart + Item^.Offset;
    WriteData(FStream, Dest, Item^.Size);
  end;
end;

function TAsarArchive.VerifyItem(Index: Integer): Integer;
var
  Read: Integer;
  Item: PAsarItem;
  Ctx: TDCP_sha256;
  Dst: TMemoryStream;
  Digest: array[0..31] of byte;
  Buf: array[0..BUF_SIZE] of Byte;
begin
  Item:= FItems[Index];

  if Item^.SHA256 = '' then
    Exit(E_NOT_SUPPORTED);

  Dst:= TMemoryStream.Create;
  try
    ExtractItem(Index, Dst);
    Dst.Position:= 0;
    Ctx:= TDCP_sha256.Create(nil);
    Ctx.Init;
    while Dst.Position < Dst.Size do begin
      Read:= Dst.Read(Buf, SizeOf(Buf));
      Ctx.Update(Buf, Read);
    end;
    Ctx.Final(Digest);
    if SameText(DigestToHex(Digest), Item^.SHA256) then
      Result:= E_SUCCESS
    else
      Result:= E_BAD_DATA;
  finally
    Dst.Free;
  end;
end;

procedure TAsarArchive.AddFile(const DiskPath, AsarPath: string; External: Boolean = False; Executable: Boolean = False);
var
  NewItem: PAsarItem;
begin
  New(NewItem);
  NewItem^.FileName:= AsarPath;
  NewItem^.DiskPath:= DiskPath;
  NewItem^.IsExternal:= External;
  NewItem^.IsExecutable:= Executable;
  NewItem^.IsDir:= False;
  FItems.Add(NewItem);
end;

procedure TAsarArchive.AddDir(const AsarPath: string);
var
  NewItem: PAsarItem;
begin
  New(NewItem);
  NewItem^.FileName:= AsarPath;
  NewItem^.DiskPath:= '';
  NewItem^.Size:= -1;
  NewItem^.IsExternal:= False;
  NewItem^.IsExecutable:= False;
  NewItem^.IsDir:= True;
  FItems.Add(NewItem);
end;

procedure TAsarArchive.RemoveFile(const AsarPathMask: string);
var
  i: Integer;
begin
  for i:= FItems.Count - 1 downto 0 do
    if MatchesMask(PAsarItem(FItems[i])^.FileName, AsarPathMask, (PathDelim = '/')) then
    begin
      Dispose(PAsarItem(FItems[i]));
      FItems.Delete(i);
    end;
end;

procedure TAsarArchive.WritePickle(Stream: TStream; JsonSize: Cardinal);
var
  u: Cardinal;
begin
  u:= 4;
  Stream.WriteBuffer(u, 4);
  u:= JsonSize + 8;
  Stream.WriteBuffer(u, 4);
  u:= JsonSize + 4;
  Stream.WriteBuffer(u, 4);
  u:= JsonSize;
  Stream.WriteBuffer(u, 4);
end;

procedure TAsarArchive.WritePadding(Stream: TStream);
begin
  while (Stream.Position mod 4) <> 0 do
    Stream.WriteByte(0);
end;

procedure TAsarArchive.AddToTree(Root: TJSONObject; const VirtualPath: string; FileObj: TJSONObject);
var
  I: Integer;
  PartName: string;
  Parts: TStringArray;
  CurrentNode, FilesNode: TJSONObject;
begin
  CurrentNode:= Root;
  Parts:= VirtualPath.Split(['/'], TStringSplitOptions.ExcludeEmpty);


  for I:= 0 to High(Parts) do
  begin
    PartName:= Parts[i];
    FilesNode:= CurrentNode.Find('files') as TJSONObject;

    if not Assigned(FilesNode) then
    begin
      FilesNode:= TJSONObject.Create;
      CurrentNode.Add('files', FilesNode);
    end;

    if I = High(Parts) then
    begin
      if Assigned(FileObj) then
        FilesNode.Add(PartName, FileObj)
      else if not Assigned(FilesNode.Find(PartName)) then
        FilesNode.Add(PartName, TJSONObject.Create);
    end
    else
    begin
      if not Assigned(FilesNode.Find(PartName)) then
        FilesNode.Add(PartName, TJSONObject.Create);

      CurrentNode:= FilesNode.Objects[PartName];
    end;
  end;
end;

function TAsarArchive.SaveTo(const FileName: string): Integer;
var
  I: Integer;
  Item: PAsarItem;
  CurOff: Int64 = 0;
  Blocks: TJSONArray;
  Dst, Src, External: TStream;
  Root, FilesNode, CurObj, Intgr: TJSONObject;
  JsonStr, Hash, TmpName, UnpackedDir, DestPath: string;
begin
  Result:= E_SUCCESS;
  UnpackedDir:= FileName + '.unpacked';
  TmpName:= ChangeFileExt(FileName, '') + FormatDateTime('_yyyymmdd_hhnn', Now) + '.tmp';
  try
    Dst:= TFileStream.Create(TmpName, fmCreate);
    try
      Root:= TJSONObject.Create;
      FilesNode:= TJSONObject.Create;
      for I:= 0 to FItems.Count - 1 do
      begin
        Item:= FItems[I];
        if Item^.IsDir then
        begin
          CurObj:= TJSONObject.Create;
          AddToTree(Root, Item^.FileName, CurObj);
          continue;
        end;

        if Item^.DiskPath <> '' then
        begin
          Item^.Size:= mbFileSize(Item^.DiskPath);
          CalculateIntegrity(Item^.DiskPath, Hash, Blocks);
        end
        else
        begin
          Hash:= Item^.SHA256;
          Blocks:= nil;
        end;

        CurObj:= TJSONObject.Create;
        CurObj.Add('size', Item^.Size);
        if Item^.IsExternal then
          CurObj.Add('unpacked', True)
        else
        begin
          CurObj.Add('offset', IntToStr(CurOff));
          CurOff:= Align4(CurOff + Item^.Size);
        end;

        if Hash <> '' then
        begin
          Intgr:= TJSONObject.Create;
          Intgr.Add('algorithm', 'SHA256');
          Intgr.Add('hash', Hash);
          if Assigned(Blocks) then
            Intgr.Add('blocks', Blocks);
          CurObj.Add('integrity', Intgr);
        end;

        if Item^.IsExecutable then
          CurObj.Add('executable', True);

        AddToTree(Root, Item^.FileName, CurObj);
      end;

      JsonStr:= Root.AsJSON;
      WritePickle(Dst, Length(JsonStr));
      Dst.WriteBuffer(Pointer(JsonStr)^, Length(JsonStr));
      WritePadding(Dst);

      for I:= 0 to FItems.Count - 1 do
      begin
        Item:= FItems[I];
        ProcFile:= Item^.FileName;
        if Item^.IsDir then
        begin
          continue;
        end;

        if Item^.DiskPath <> '' then
          Src:= TFileStream.Create(Item^.DiskPath, fmOpenRead)
        else
        begin
          Src:= TMemoryStream.Create;
          ExtractItem(I, Src);
          Src.Position:= 0;
        end;

        if Item^.IsExternal then
        begin
          DestPath:= UnpackedDir + PathDelim + StringReplace(Item^.FileName, '/', PathDelim, [rfReplaceAll]);
          ForceDirectories(ExtractFilePath(DestPath));
          External:= TFileStream.Create(DestPath, fmCreate);
          Result:= WriteData(Src, External, Src.Size);
          External.Free;
        end
        else
        begin
          Result:= WriteData(Src, Dst, Src.Size);
          WritePadding(Dst);
        end;
        Src.Free;
        if Result <> E_SUCCESS then
          break;
      end;
    finally
      Root.Free;
      Dst.Free;

      if Result = E_SUCCESS then
      begin
        DeleteFile(FileName);
        RenameFile(TmpName, FileName);
      end
      else
        DeleteFile(TmpName);
    end;
  except
    Result:= E_EWRITE;
  end;
end;

end.
