unit uAsar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, DCPsha256, StrUtils,
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
    LinkTarget : string;
    IsExternal: Boolean;
    IsExecutable: Boolean;
    IsDir: Boolean;
    IsLink: Boolean;
    SHA256: string;
    Integrity: TJSONObject;
  end;
  PAsarItem = ^TAsarItem;

  { TAsarArchive }

  TAsarArchive = class
  private
    FStream: TFileStream;
    FItems: TFPList;
    FDataStart: Int64;
    FProcFile: UnicodeString;
    FStoreHash: Boolean;
    FCloseAfterSave: Boolean;
    FProcessDataProc: TProcessDataProcW;

    procedure ClearItems;
    procedure ParseJson(Obj: TJSONObject; const ParentPath: string);
    procedure AddToTree(Root: TJSONObject; const VirtualPath: string; FileObj: TJSONObject);
    function Align4(Val: Int64): Int64;
    procedure WritePickle(Stream: TStream; JsonSize: Cardinal);
    procedure WritePadding(Stream: TStream);
    function DigestToHex(const D: array of byte): string;
    function CalculateIntegrity(const AFilePath: string; out FullHash: string; out Blocks: TJSONArray): Integer;
    function GetCount: Integer;
    procedure SetProc(ProcessDataProc: TProcessDataProcW);
    procedure SetProcFile(FileName: string);
    function GetItem(Index: Integer): TAsarItem;
    function WriteData(Src, Dst: TStream; Size: Int64): Integer;
    function GetProgress(Stream: TStream): Integer;
    procedure RemoveSpaces(var JsonString: string);
    function RemoveTrailingSlash(const Path: string): string;
    function InitItem(const AsarPath: string; RemoveOld: Boolean = True): PAsarItem;
    function IsUpdatingSelf(const FileName: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function Open(const FileName: string): Boolean;
    procedure Close;

    property ProcessDataProc: TProcessDataProcW write SetProc;
    property ProcFile: string write SetProcFile;
    property StoreHash: Boolean read FStoreHash write FStoreHash;
    property CloseAfterSave: Boolean read FCloseAfterSave write FCloseAfterSave;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TAsarItem read GetItem; default;

    procedure Sort;

    function ExtractItem(Index: Integer; Dest: TStream): Integer;
    function VerifyItem(Index: Integer): Integer;

    procedure AddFile(const DiskPath, AsarPath: string; External: Boolean = False; Executable: Boolean = False);
    procedure AddLink(const LinkTarget, AsarPath: string; RemoveOld: Boolean = True);
    procedure AddDir(const AsarPath: string);
    procedure RemoveFile(const AsarPath: string);
    function SaveTo(const FileName: string): Integer;
  end;

implementation

constructor TAsarArchive.Create;
begin
  FItems:= TFPList.Create;
  FProcFile:= '';
  FStoreHash:= True;
  FCloseAfterSave:= False;
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
  begin
    if Assigned(PAsarItem(FItems[I])^.Integrity) then
      PAsarItem(FItems[I])^.Integrity.Free;
    Dispose(PAsarItem(FItems[I]));
  end;
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

function TAsarArchive.IsUpdatingSelf(const FileName: string): Boolean;
begin
  Result:= False;
  if Assigned(FStream) then
    Result:= SameFileName(ExpandFileName(FStream.FileName), ExpandFileName(FileName));
end;

procedure TAsarArchive.SetProc(ProcessDataProc: TProcessDataProcW);
begin
  FProcessDataProc:= ProcessDataProc;
end;

procedure TAsarArchive.SetProcFile(FileName: string);
begin
  FProcFile:= CeUtf8ToUtf16(FileName);
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

      if Assigned(FProcessDataProc) and
         (FProcessDataProc(PWideChar(FProcFile), Read) = 0) then
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

function TAsarArchive.GetProgress(Stream: TStream): Integer;
const
  FirstBar = 1000;
var
  Percent: Integer;
begin
  Percent:= 0;
  if Stream.Size > 0 then
    Percent:= Round(Stream.Position * 100 / Stream.Size);
  Result:= -(FirstBar + Percent)
end;

procedure TAsarArchive.RemoveSpaces(var JsonString: string);
var
  I: integer = 1;
  Quoted: boolean = False;
begin
  while I <= Length(JsonString) do
  begin
    if (JsonString[i] = '"') and ((I = 1) or (JsonString[I - 1] <> '\')) then
      Quoted:= not Quoted;

    if (JsonString[I] = ' ') and (not Quoted) then
      Delete(JsonString, I, 1)
    else
      Inc(I);
  end;
end;

function TAsarArchive.RemoveTrailingSlash(const Path: string): string;
begin
  Result:= Path;
  while (Result <> '') and (Result[Length(Result)] = '/') do
    Delete(Result, Length(Result), 1);
end;

function TAsarArchive.CalculateIntegrity(const AFilePath: string; out FullHash: string; out Blocks: TJSONArray): Integer;
var
  Src: TFileStream;
  FullCtx, BlockCtx: TDCP_sha256;
  FullDigest, BlockDigest: array[0..31] of byte;
  Buf: array[0..BUF_SIZE] of Byte;
  Read, BytesInBlock: Integer;
begin
  Result:= E_SUCCESS;
  Blocks:= TJSONArray.Create;
  Src:= TFileStream.Create(AFilePath, fmOpenRead or fmShareDenyNone);
  try
    BytesInBlock:= 0;
    FullCtx:= TDCP_sha256.Create(nil);
    BlockCtx:= TDCP_sha256.Create(nil);
    FullCtx.Init;
    BlockCtx.Init;

    if Src.Size = 0 then  // ¯\_(ツ)_/¯
    begin
      BlockCtx.Final(BlockDigest);
      Blocks.Add(DigestToHex(BlockDigest));
      FullCtx.Final(FullDigest);
      FullHash := DigestToHex(FullDigest);
      Exit;
    end;

    while Src.Position < Src.Size do begin
      if Assigned(FProcessDataProc) and
         (FProcessDataProc(PWideChar(FProcFile), GetProgress(Src)) = 0) then
      begin
        Result:= E_EABORTED;
        break;
      end;
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
    if Result = E_SUCCESS then
    begin
      FullCtx.Final(FullDigest);
      FullHash:= DigestToHex(FullDigest);
    end;
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

  //if (FilesNode.Count = 0) and (ParentPath <> '') then
  if (ParentPath <> '') then
    AddDir(ParentPath);

  for I:= 0 to FilesNode.Count - 1 do
  begin
    Name:= FilesNode.Names[I];
    Item:= FilesNode.Items[I] as TJSONObject;
    if Assigned(Item.Find('files')) then
      ParseJson(Item, ParentPath + Name + '/')
    else if Assigned(Item.Find('link')) then
      AddLink(Item.Strings['link'], ParentPath + Name, False)
    else
    begin
      NewItem:= InitItem(ParentPath + Name, False);
      NewItem^.Size:= Item.Int64s['size'];

      if Item.Find('executable') <> nil then
        NewItem^.IsExecutable:= Item.Booleans['executable']
      else
        NewItem^.IsExecutable:= False;

      if Item.Find('unpacked') <> nil then
        NewItem^.IsExternal:= Item.Booleans['unpacked'];

      if not NewItem^.IsExternal then
        NewItem^.Offset:= StrToInt64(Item.Strings['offset']);

      Intgr:= Item.Find('integrity') as TJSONObject;
      if Assigned(Intgr) then
      begin
        NewItem^.SHA256:= Intgr.Strings['hash'];
        NewItem^.Integrity := Intgr.Clone as TJSONObject;
      end;

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

function CompareAsarItems(Item1, Item2: Pointer): Integer;
begin
  Result:= CompareText(PAsarItem(Item1)^.FileName, PAsarItem(Item2)^.FileName);
end;

procedure TAsarArchive.Sort;
begin
  if FItems.Count > 1 then
    FItems.Sort(@CompareAsarItems);
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

function TAsarArchive.InitItem(const AsarPath: string; RemoveOld: Boolean = True): PAsarItem;
begin
  if RemoveOld then
    RemoveFile(AsarPath);

  New(Result);
  Result^.FileName:= AsarPath;
  Result^.Size:= 0;
  Result^.Offset:= -1;
  Result^.DiskPath:= '';
  Result^.SHA256:= '';
  Result^.LinkTarget:= '';
  Result^.IsExternal:= False;
  Result^.IsDir:= False;
  Result^.IsLink:= False;
  Result^.IsExecutable:= False;
  Result^.Integrity:= nil;
end;

procedure TAsarArchive.AddFile(const DiskPath, AsarPath: string; External: Boolean = False; Executable: Boolean = False);
var
  NewItem: PAsarItem;
begin
  NewItem:= InitItem(AsarPath);
  NewItem^.DiskPath:= DiskPath;
  NewItem^.IsExternal:= External;
  NewItem^.IsExecutable:= Executable;
  FItems.Add(NewItem);
end;

procedure TAsarArchive.AddLink(const LinkTarget, AsarPath: string; RemoveOld: Boolean = True);
var
  NewItem: PAsarItem;
begin
  NewItem:= InitItem(AsarPath, RemoveOld);
  NewItem^.FileName:= AsarPath;
  NewItem^.LinkTarget:= LinkTarget;
  NewItem^.IsLink:= True;
  FItems.Add(NewItem);
end;

procedure TAsarArchive.AddDir(const AsarPath: string);
var
  Path: string;
  NewItem: PAsarItem;
begin
  Path:= RemoveTrailingSlash(AsarPath);
  NewItem:= InitItem(Path, False);
  NewItem^.IsDir:= True;
  FItems.Add(NewItem);
end;

procedure TAsarArchive.RemoveFile(const AsarPath: string);
var
  i: Integer;
  Path, Prefix: string;
begin
  if EndsText('/*.*', AsarPath) then
  begin
    Path:= Copy(AsarPath, 1, Length(AsarPath) - 4);
    Prefix:= Copy(AsarPath, 1, Length(AsarPath) - 3);
  end
  else
  begin
    Path:= AsarPath;
    Prefix:= Path + '/';
  end;
  for i:= FItems.Count - 1 downto 0 do
  begin
    if StartsText(Prefix, PAsarItem(FItems[i])^.FileName) or
       SameText(PAsarItem(FItems[i])^.FileName, Path) then
    begin
      if Assigned(PAsarItem(FItems[i])^.Integrity) then
        PAsarItem(FItems[i])^.Integrity.Free;
      Dispose(PAsarItem(FItems[i]));
      FItems.Delete(i);
    end;
  end;
end;

procedure TAsarArchive.WritePickle(Stream: TStream; JsonSize: Cardinal);
var
  u, AlignedJsonSize: Cardinal;
begin
  AlignedJsonSize := (JsonSize + 3) and not 3;
  u := 4;
  Stream.WriteBuffer(u, 4);
  u := AlignedJsonSize + 8;
  Stream.WriteBuffer(u, 4);
  u := AlignedJsonSize + 4;
  Stream.WriteBuffer(u, 4);
  u := JsonSize;
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
  CurrentNode, FilesNode, Existing: TJSONObject;
begin
  CurrentNode:= Root;
  Parts:= VirtualPath.Split(['/'], TStringSplitOptions.ExcludeEmpty);
  if Length(Parts) = 0 then
  begin
    FileObj.Free;
    Exit;
  end;

  for I:= 0 to High(Parts) do
  begin
    PartName:= Parts[i];
    FilesNode:= CurrentNode.Find('files') as TJSONObject;

    if not Assigned(FilesNode) then
    begin
      FilesNode:= TJSONObject.Create;
      CurrentNode.Add('files', FilesNode);
    end;

    Existing:= FilesNode.Find(PartName) as TJSONObject;

    if I = High(Parts) then
    begin
      if Assigned(Existing) then
      begin
        if (FileObj.Find('files') <> nil) then
        begin
          if not Assigned(Existing.Find('files')) then
          begin
            Existing.Clear;
            Existing.Add('files', TJSONObject.Create);
          end;
          FileObj.Free;
        end
        else
        begin
          FilesNode.Delete(PartName);
          FilesNode.Add(PartName, FileObj);
        end;
      end
      else
        FilesNode.Add(PartName, FileObj);
    end
    else
    begin
      if not Assigned(Existing) then
      begin
        Existing:= TJSONObject.Create;
        FilesNode.Add(PartName, Existing);
      end;

      CurrentNode:= Existing;
    end;
  end;
end;

function TAsarArchive.SaveTo(const FileName: string): Integer;
var
  I: integer;
  Needed: Int64;
  Item: PAsarItem;
  CurOff: int64 = 0;
  Blocks: TJSONArray;
  Dst, Src, External: TStream;
  Root, FilesNode, CurObj, Intgr: TJSONObject;
  JsonStr, Hash, TmpName, UnpackedDir, DestPath: string;
  SelfUpdate: Boolean;
begin
  Result:= E_SUCCESS;
  UnpackedDir:= FileName + '.unpacked';
  SelfUpdate:= IsUpdatingSelf(FileName);
  TmpName:= ChangeFileExt(FileName, '') + FormatDateTime('_yyyymmdd_hhnn', Now) + '.tmp';

  Dst:= TFileStream.Create(TmpName, fmCreate);
  try
    Root:= TJSONObject.Create;
    FilesNode:= TJSONObject.Create;
    for I:= 0 to FItems.Count - 1 do
    begin
      Item:= FItems[I];
      CurObj:= TJSONObject.Create;

      if Item^.IsDir then
        CurObj.Add('files', TJSONObject.Create)
      else if Item^.IsLink then
        CurObj.Add('link', Item^.LinkTarget)
      else
      begin
        if (Item^.DiskPath <> '') then
          Item^.Size:= mbFileSize(Item^.DiskPath);

        CurObj.Add('size', Item^.Size);
        if Item^.IsExternal then
          CurObj.Add('unpacked', True)
        else
        begin
          CurObj.Add('offset', IntToStr(CurOff));
          //CurOff:= Align4(CurOff + Item^.Size);
          CurOff:= CurOff + Item^.Size;
        end;

        if StoreHash then
        begin
          if (Item^.DiskPath <> '') then
          begin
            ProcFile:= Item^.FileName;
            Result:= CalculateIntegrity(Item^.DiskPath, Hash, Blocks);

            if Result <> E_SUCCESS then
              break;

            Intgr:= TJSONObject.Create;
            Intgr.Add('algorithm', 'SHA256');
            Intgr.Add('hash', Hash);
            if Assigned(Blocks) then
            begin
              Intgr.Add('blockSize', ASAR_BLOCK_SIZE);
              Intgr.Add('blocks', Blocks);
            end;
            CurObj.Add('integrity', Intgr);
          end
          else if Assigned(Item^.Integrity) then
            CurObj.Add('integrity', Item^.Integrity.Clone);
        end;

        if not Item^.IsLink and Item^.IsExecutable then
          CurObj.Add('executable', True);
      end;

      AddToTree(Root, Item^.FileName, CurObj);
    end;

    if Result = E_SUCCESS then
    begin
      JsonStr:= Root.AsJSON;
      RemoveSpaces(JsonStr);

      Needed:= 16 + Align4(Length(JsonStr)) + CurOff;
      Dst.Size:= Needed;
      Dst.Position:= 0;

      if Dst.Size < Needed then
      begin
        Result:= E_ECREATE;
        raise Exception.Create(EmptyStr);
      end;

      WritePickle(Dst, Length(JsonStr));
      Dst.WriteBuffer(Pointer(JsonStr)^, Length(JsonStr));
      WritePadding(Dst);

      for I:= 0 to FItems.Count - 1 do
      begin
        Item:= FItems[I];
        ProcFile:= Item^.FileName;
        if Item^.IsDir or Item^.IsLink then
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
          DestPath:= UnpackedDir + PathDelim +
            StringReplace(Item^.FileName, '/', PathDelim, [rfReplaceAll]);
          ForceDirectories(ExtractFilePath(DestPath));
          External:= TFileStream.Create(DestPath, fmCreate);
          Result:= WriteData(Src, External, Src.Size);
          External.Free;
        end
        else
        begin
          Result:= WriteData(Src, Dst, Src.Size);
          //WritePadding(Dst);
        end;
        Src.Free;
        if Result <> E_SUCCESS then
          break;
      end;
    end;
  finally
    Root.Free;
    Dst.Free;
  end;

  if SelfUpdate or CloseAfterSave then
    Close;

  if Result = E_SUCCESS then
  begin
    if FileExists(FileName) then
      DeleteFile(FileName);

    if not RenameFile(TmpName, FileName) then
    begin
      DeleteFile(TmpName);
      //raise Exception.Create(EmptyStr);
      Exit(E_EWRITE);
    end;

    if SelfUpdate and not CloseAfterSave then
      Open(FileName);
  end
  else
    DeleteFile(TmpName);
end;

end.
