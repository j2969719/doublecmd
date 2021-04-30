unit fOptionsColorsGroup;

{$mode objfpc}{$H+}

interface

uses
  //Lazarus, Free-Pascal, etc.
  Graphics, Classes, SysUtils, ComCtrls, StdCtrls, ColorBox, ExtCtrls, Dialogs,

  //DC
  uColumns, fOptionsFrame, uSynDiffControls
  {$IFDEF COLUMNSFILEVIEW_VTV}
  , uColumnsFileViewVtv
  {$ELSE}
  , uColumnsFileView
  {$ENDIF}
  ;

type

  { TfrmOptionsColorsGroup }

  TfrmOptionsColorsGroup = class(TOptionsEditor)
    btnIndBackColor: TButton;
    btnIndColor: TButton;
    btnResetToDCDefault: TButton;
    btnActiveText: TButton;
    btnActiveBg: TButton;
    btnInactiveText: TButton;
    btnInactiveBg: TButton;
    btnInfoColor: TButton;
    btnErrorColor: TButton;
    btnSuccessColor: TButton;
    btnAdded: TButton;
    btnDeleted: TButton;
    btnModified: TButton;
    btnLeft: TButton;
    btnRight: TButton;
    btnUnknown: TButton;
    cbbUseGradientInd: TCheckBox;
    cbIndBackColor: TColorBox;
    cbIndColor: TColorBox;
    cbActiveText: TColorBox;
    cbActiveBg: TColorBox;
    cbInactiveText: TColorBox;
    cbInactiveBg: TColorBox;
    cbInfoColor: TColorBox;
    cbErrorColor: TColorBox;
    cbSuccessColor: TColorBox;
    cbAdded: TColorBox;
    cbDeleted: TColorBox;
    cbModified: TColorBox;
    cbLeft: TColorBox;
    cbRight: TColorBox;
    cbUnknown: TColorBox;
    gbFreeSpaceIndicator: TGroupBox;
    gbDiffer: TGroupBox;
    gbSyncDirs: TGroupBox;
    gbPathLabel: TGroupBox;
    gbLogWindow: TGroupBox;
    lblLeftColor: TLabel;
    lblRightColor: TLabel;
    lblUnknownColor: TLabel;
    lblAddedColor: TLabel;
    lblDeletedColor: TLabel;
    lblModifiedColor: TLabel;
    lblDeleted: TLabel;
    lblRight: TLabel;
    lblInfo: TLabel;
    lblError: TLabel;
    lblAdded: TLabel;
    lblLeft: TLabel;
    lblSuccess: TLabel;
    lblInfoColor: TLabel;
    lblErrorColor: TLabel;
    lblSuccessColor: TLabel;
    lblActivePath: TLabel;
    lblInactivePath: TLabel;
    lblActiveText: TLabel;
    lblActiveBg: TLabel;
    lblInaciveText: TLabel;
    lblInactiveBg: TLabel;
    lblIndBackColor: TLabel;
    lblIndColor: TLabel;
    lblModified: TLabel;
    lblUnknown: TLabel;
    optColorDialog: TColorDialog;
    pLogColors: TPanel;
    pDifferColors: TPanel;
    pSyncDirsColors: TPanel;
    pLogPreview: TPanel;
    pDifferPreview: TPanel;
    pSyncDirsPreview: TPanel;
    pPathPreview: TPanel;
    pPathColors: TPanel;
    pbxFakeDrive: TPaintBox;
    procedure btnActiveBgClick(Sender: TObject);
    procedure btnActiveTextClick(Sender: TObject);
    procedure btnAddedClick(Sender: TObject);
    procedure btnDeletedClick(Sender: TObject);
    procedure btnErrorColorClick(Sender: TObject);
    procedure btnInactiveBgClick(Sender: TObject);
    procedure btnInactiveTextClick(Sender: TObject);
    procedure btnIndBackColorClick(Sender: TObject);
    procedure btnIndColorClick(Sender: TObject);
    procedure btnInfoColorClick(Sender: TObject);
    procedure btnLeftClick(Sender: TObject);
    procedure btnModifiedClick(Sender: TObject);
    procedure btnResetToDCDefaultClick(Sender: TObject);
    procedure btnRightClick(Sender: TObject);
    procedure btnSuccessColorClick(Sender: TObject);
    procedure btnUnknownClick(Sender: TObject);
    procedure cbActiveBgChange(Sender: TObject);
    procedure cbActiveTextChange(Sender: TObject);
    procedure cbAddedChange(Sender: TObject);
    procedure cbbUseGradientIndChange(Sender: TObject);
    procedure cbDeletedChange(Sender: TObject);
    procedure cbErrorColorChange(Sender: TObject);
    procedure cbInactiveBgChange(Sender: TObject);
    procedure cbInactiveTextChange(Sender: TObject);
    procedure cbIndBackColorChange(Sender: TObject);
    procedure cbIndColorChange(Sender: TObject);
    procedure cbInfoColorChange(Sender: TObject);
    procedure cbLeftChange(Sender: TObject);
    procedure cbModifiedChange(Sender: TObject);
    procedure cbRightChange(Sender: TObject);
    procedure cbSuccessColorChange(Sender: TObject);
    procedure cbUnknownChange(Sender: TObject);
    procedure pbxFakeDrivePaint(Sender: TObject);
  private
    bLoadCompleted: boolean;
  protected
    procedure Init; override;
    procedure Load; override;
    function Save: TOptionsEditorSaveFlags; override;
  public
    class function GetIconIndex: Integer; override;
    class function GetTitle: String; override;
  end;

implementation

{$R *.lfm}

uses
  //Lazarus, Free-Pascal, etc.
  Forms,

  //DC
  uSampleForConfigFileSource, uFileFunctions, DCOSUtils, fMain, uLng, uGlobs,
  uDCUtils;

{ TfrmOptionsColorsGroup }

class function TfrmOptionsColorsGroup.GetIconIndex: Integer;
begin
  Result := 4;
end;

class function TfrmOptionsColorsGroup.GetTitle: String;
begin
  Result := rsOptionsEditorColors;
end;

procedure TfrmOptionsColorsGroup.Init;
begin

end;

procedure TfrmOptionsColorsGroup.Load;
begin
  bLoadCompleted := False;

  cbbUseGradientInd.Checked := gIndUseGradient;
  SetColorInColorBox(cbIndColor, gIndForeColor);
  SetColorInColorBox(cbIndBackColor, gIndBackColor);

  SetColorInColorBox(cbActiveText, gPathActiveFontColor);
  SetColorInColorBox(cbActiveBg, gPathActiveColor);
  SetColorInColorBox(cbInactiveText, gPathInactiveFontColor);
  SetColorInColorBox(cbInactiveBg, gPathInactiveColor);

  SetColorInColorBox(cbInfoColor, gLogInfoColor);
  SetColorInColorBox(cbErrorColor, gLogErrorColor);
  SetColorInColorBox(cbSuccessColor, gLogSuccessColor);
  lblInfo.Font.Name := gFonts[dcfLog].Name;
  lblInfo.Font.Size := gFonts[dcfLog].Size;
  lblError.Font.Name := gFonts[dcfLog].Name;
  lblError.Font.Size := gFonts[dcfLog].Size;
  lblSuccess.Font.Name := gFonts[dcfLog].Name;
  lblSuccess.Font.Size := gFonts[dcfLog].Size;

  SetColorInColorBox(cbAdded, gDifferAddedColor);
  SetColorInColorBox(cbDeleted, gDifferDeletedColor);
  SetColorInColorBox(cbModified, gDifferModifiedColor);
  lblAdded.Font.Name := gFonts[dcfEditor].Name;
  lblAdded.Font.Size := gFonts[dcfEditor].Size;
  lblDeleted.Font.Name := gFonts[dcfEditor].Name;
  lblDeleted.Font.Size := gFonts[dcfEditor].Size;
  lblModified.Font.Name := gFonts[dcfEditor].Name;
  lblModified.Font.Size := gFonts[dcfEditor].Size;

  SetColorInColorBox(cbLeft, gSyncLeftColor);
  SetColorInColorBox(cbRight, gSyncRightColor);
  SetColorInColorBox(cbUnknown, gSyncUnknownColor);

  bLoadCompleted := True;
end;

function TfrmOptionsColorsGroup.Save: TOptionsEditorSaveFlags;
begin
  gIndUseGradient := cbbUseGradientInd.Checked;
  gIndForeColor := cbIndColor.Selected;
  gIndBackColor := cbIndBackColor.Selected;

  gPathActiveFontColor := cbActiveText.Selected;
  gPathActiveColor := cbActiveBg.Selected;
  gPathInactiveFontColor := cbInactiveText.Selected;
  gPathInactiveColor := cbInactiveBg.Selected;

  gLogInfoColor := cbInfoColor.Selected;
  gLogErrorColor := cbErrorColor.Selected;
  gLogSuccessColor := cbSuccessColor.Selected;

  gDifferAddedColor := cbAdded.Selected;
  gDifferDeletedColor := cbDeleted.Selected;
  gDifferModifiedColor := cbModified.Selected;

  gSyncLeftColor := cbLeft.Selected;
  gSyncRightColor := cbRight.Selected;
  gSyncUnknownColor := cbUnknown.Selected;

  Result := [];
end;

{ TfrmOptionsColorsGroup.cbbUseGradientIndChange }
procedure TfrmOptionsColorsGroup.cbbUseGradientIndChange(Sender: TObject);
begin
  lblIndColor.Enabled := not (cbbUseGradientInd.Checked);
  lblIndBackColor.Enabled := not (cbbUseGradientInd.Checked);
  cbIndColor.Enabled := not (cbbUseGradientInd.Checked);
  cbIndBackColor.Enabled := not (cbbUseGradientInd.Checked);
  btnIndColor.Enabled := not (cbbUseGradientInd.Checked);
  btnIndBackColor.Enabled := not (cbbUseGradientInd.Checked);
  pbxFakeDrive.Repaint;
end;

procedure TfrmOptionsColorsGroup.cbDeletedChange(Sender: TObject);
begin
  lblDeleted.Color:=cbDeleted.Selected;
end;

procedure TfrmOptionsColorsGroup.cbErrorColorChange(Sender: TObject);
begin
  lblError.Font.Color:=cbErrorColor.Selected;
end;

procedure TfrmOptionsColorsGroup.cbInactiveBgChange(Sender: TObject);
begin
  lblInactivePath.Color:=cbInactiveBg.Selected;
end;

procedure TfrmOptionsColorsGroup.cbInactiveTextChange(Sender: TObject);
begin
  lblInactivePath.Font.Color:=cbInactiveText.Selected;
end;

{ TfrmOptionsColorsGroup.cbIndBackColorChange }
procedure TfrmOptionsColorsGroup.cbIndBackColorChange(Sender: TObject);
begin
  pbxFakeDrive.Repaint;
end;

{ TfrmOptionsColorsGroup.cbIndColorChange }
procedure TfrmOptionsColorsGroup.cbIndColorChange(Sender: TObject);
begin
  pbxFakeDrive.Repaint;
end;

procedure TfrmOptionsColorsGroup.cbInfoColorChange(Sender: TObject);
begin
  lblInfo.Font.Color:=cbInfoColor.Selected;
end;

procedure TfrmOptionsColorsGroup.cbLeftChange(Sender: TObject);
begin
  lblLeft.Font.Color:=cbLeft.Selected;
end;

procedure TfrmOptionsColorsGroup.cbModifiedChange(Sender: TObject);
begin
  lblModified.Color:=cbModified.Selected;
end;

procedure TfrmOptionsColorsGroup.cbRightChange(Sender: TObject);
begin
  lblRight.Font.Color:=cbRight.Selected;
end;

procedure TfrmOptionsColorsGroup.cbSuccessColorChange(Sender: TObject);
begin
  lblSuccess.Font.Color:=cbSuccessColor.Selected;
end;

procedure TfrmOptionsColorsGroup.cbUnknownChange(Sender: TObject);
begin
  lblUnknown.Font.Color:=cbUnknown.Selected;
end;

procedure TfrmOptionsColorsGroup.pbxFakeDrivePaint(Sender: TObject);
begin
  frmMain.PaintDriveFreeBar(pbxFakeDrive, cbbUseGradientInd.Checked, cbIndColor.Selected, cbIndBackColor.Selected);
end;

{ TfrmOptionsColorsGroup.btnIndColorClick }
procedure TfrmOptionsColorsGroup.btnIndColorClick(Sender: TObject);
begin
  optColorDialog.Color := cbIndColor.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbIndColor, optColorDialog.Color);
    pbxFakeDrive.Repaint;
  end;
end;

procedure TfrmOptionsColorsGroup.btnInfoColorClick(Sender: TObject);
begin
  optColorDialog.Color := cbInfoColor.Selected;
  if optColorDialog.Execute then
    SetColorInColorBox(cbInfoColor, optColorDialog.Color);

end;

procedure TfrmOptionsColorsGroup.btnLeftClick(Sender: TObject);
begin
  optColorDialog.Color := cbLeft.Selected;
  if optColorDialog.Execute then
    SetColorInColorBox(cbLeft, optColorDialog.Color);
end;

procedure TfrmOptionsColorsGroup.btnModifiedClick(Sender: TObject);
begin
  optColorDialog.Color := cbModified.Selected;
  if optColorDialog.Execute then
    SetColorInColorBox(cbModified, optColorDialog.Color);
end;

procedure TfrmOptionsColorsGroup.btnResetToDCDefaultClick(Sender: TObject);
begin
  SetColorInColorBox(cbIndColor, clBlack);
  SetColorInColorBox(cbIndBackColor, clWhite);
  cbbUseGradientInd.Checked := True;

  SetColorInColorBox(cbActiveText, clHighlightText);
  SetColorInColorBox(cbActiveBg, clHighlight);
  SetColorInColorBox(cbInactiveText, clBtnText);
  SetColorInColorBox(cbInactiveBg, clBtnFace);

  SetColorInColorBox(cbInfoColor, clNavy);
  SetColorInColorBox(cbErrorColor, clRed);
  SetColorInColorBox(cbSuccessColor, clGreen);

  SetColorInColorBox(cbAdded, clPaleGreen);
  SetColorInColorBox(cbDeleted, clPaleRed);
  SetColorInColorBox(cbModified, clPaleBlue);

  SetColorInColorBox(cbLeft, clGreen);
  SetColorInColorBox(cbRight, clBlue);
  SetColorInColorBox(cbUnknown, clRed);
end;

procedure TfrmOptionsColorsGroup.btnRightClick(Sender: TObject);
begin
  optColorDialog.Color := cbRight.Selected;
  if optColorDialog.Execute then
    SetColorInColorBox(cbRight, optColorDialog.Color);
end;

procedure TfrmOptionsColorsGroup.btnSuccessColorClick(Sender: TObject);
begin
  optColorDialog.Color := cbSuccessColor.Selected;
  if optColorDialog.Execute then
    SetColorInColorBox(cbSuccessColor, optColorDialog.Color);
end;

procedure TfrmOptionsColorsGroup.btnUnknownClick(Sender: TObject);
begin
  optColorDialog.Color := cbUnknown.Selected;
  if optColorDialog.Execute then
    SetColorInColorBox(cbUnknown, optColorDialog.Color);
end;

procedure TfrmOptionsColorsGroup.cbActiveBgChange(Sender: TObject);
begin
  lblActivePath.Color:=cbActiveBg.Selected;
end;

procedure TfrmOptionsColorsGroup.cbActiveTextChange(Sender: TObject);
begin
  lblActivePath.Font.Color:=cbActiveText.Selected;
end;

procedure TfrmOptionsColorsGroup.cbAddedChange(Sender: TObject);
begin
  lblAdded.Color:=cbAdded.Selected;
end;

{ TfrmOptionsColorsGroup.btnIndBackColorClick }
procedure TfrmOptionsColorsGroup.btnIndBackColorClick(Sender: TObject);
begin
  optColorDialog.Color := cbIndBackColor.Selected;
  if optColorDialog.Execute then
  begin
    SetColorInColorBox(cbIndBackColor, optColorDialog.Color);
    pbxFakeDrive.Repaint;
  end;
end;

procedure TfrmOptionsColorsGroup.btnActiveTextClick(Sender: TObject);
begin
  optColorDialog.Color := cbActiveText.Selected;
  if optColorDialog.Execute then
    SetColorInColorBox(cbActiveText, optColorDialog.Color);
end;

procedure TfrmOptionsColorsGroup.btnAddedClick(Sender: TObject);
begin
  optColorDialog.Color := cbAdded.Selected;
  if optColorDialog.Execute then
    SetColorInColorBox(cbAdded, optColorDialog.Color);
end;

procedure TfrmOptionsColorsGroup.btnDeletedClick(Sender: TObject);
begin
  optColorDialog.Color := cbDeleted.Selected;
  if optColorDialog.Execute then
    SetColorInColorBox(cbDeleted, optColorDialog.Color);
end;

procedure TfrmOptionsColorsGroup.btnErrorColorClick(Sender: TObject);
begin
  optColorDialog.Color := cbErrorColor.Selected;
  if optColorDialog.Execute then
    SetColorInColorBox(cbErrorColor, optColorDialog.Color);
end;

procedure TfrmOptionsColorsGroup.btnInactiveBgClick(Sender: TObject);
begin
  optColorDialog.Color := cbInactiveBg.Selected;
  if optColorDialog.Execute then
    SetColorInColorBox(cbInactiveBg, optColorDialog.Color);
end;

procedure TfrmOptionsColorsGroup.btnInactiveTextClick(Sender: TObject);
begin
  optColorDialog.Color := cbInactiveText.Selected;
  if optColorDialog.Execute then
    SetColorInColorBox(cbInactiveText, optColorDialog.Color);

end;

procedure TfrmOptionsColorsGroup.btnActiveBgClick(Sender: TObject);
begin
  optColorDialog.Color := cbActiveBg.Selected;
  if optColorDialog.Execute then
    SetColorInColorBox(cbActiveBg, optColorDialog.Color);

end;

end.

