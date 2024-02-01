
local my = require("common")

lfm_data = [[
object DialogBox: TDialogBox
  Left = 303
  Height = 363
  Top = 158
  Width = 559
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Options'
  ChildSizing.LeftRightSpacing = 10
  ChildSizing.TopBottomSpacing = 10
  ClientHeight = 363
  ClientWidth = 559
  OnShow = DialogBoxShow
  Position = poScreenCenter
  LCLVersion = '2.0.7.0'
  object mArcSupported: TMemo
    AnchorSideTop.Control = dbSupported
    AnchorSideTop.Side = asrBottom
    Left = 16
    Height = 90
    Top = 210
    Width = 528
    BorderSpacing.Top = 5
    ReadOnly = True
    ScrollBars = ssAutoBoth
    TabOrder = 7
    TabStop = False
    WantReturns = False
    WordWrap = False
  end
  object dbMounts: TDividerBevel
    AnchorSideLeft.Control = Owner
    AnchorSideTop.Control = Owner
    AnchorSideRight.Control = Owner
    AnchorSideRight.Side = asrBottom
    Left = 10
    Height = 17
    Top = 10
    Width = 539
    Caption = 'Mounts'
    Anchors = [akTop, akLeft, akRight]
    BorderSpacing.Left = 5
    BorderSpacing.Top = 10
    BorderSpacing.Right = 5
    Font.Style = [fsBold]
    ParentFont = False
  end
  object fneAddFile: TFileNameEdit
    AnchorSideLeft.Control = mArcSupported
    AnchorSideTop.Control = dbMounts
    AnchorSideTop.Side = asrBottom
    AnchorSideRight.Side = asrBottom
    Left = 16
    Height = 24
    Top = 32
    Width = 432
    FilterIndex = 0
    HideDirectories = False
    ButtonWidth = 23
    NumGlyphs = 1
    Anchors = [akTop, akLeft, akRight]
    BorderSpacing.Top = 5
    MaxLength = 0
    TabOrder = 0
    TabStop = False
  end
  object dbSupported: TDividerBevel
    AnchorSideLeft.Control = dbMounts
    AnchorSideTop.Control = chkSymlinks
    AnchorSideTop.Side = asrBottom
    AnchorSideRight.Control = dbMounts
    AnchorSideRight.Side = asrBottom
    Left = 10
    Height = 17
    Top = 188
    Width = 539
    Caption = 'Supported archives'
    Anchors = [akTop, akLeft, akRight]
    BorderSpacing.Top = 10
    Font.Style = [fsBold]
    ParentFont = False
  end
  object btnCancel: TBitBtn
    AnchorSideTop.Control = btnOK
    AnchorSideRight.Control = btnOK
    AnchorSideBottom.Control = btnOK
    AnchorSideBottom.Side = asrBottom
    Left = 342
    Height = 30
    Top = 315
    Width = 96
    Anchors = [akTop, akRight, akBottom]
    BorderSpacing.Right = 10
    Cancel = True
    DefaultCaption = True
    Kind = bkCancel
    ModalResult = 2
    OnClick = ButtonClick
    TabOrder = 8
  end
  object btnOK: TBitBtn
    AnchorSideTop.Control = mArcSupported
    AnchorSideTop.Side = asrBottom
    AnchorSideRight.Control = mArcSupported
    AnchorSideRight.Side = asrBottom
    Left = 448
    Height = 30
    Top = 315
    Width = 96
    Anchors = [akTop, akRight]
    BorderSpacing.Top = 15
    Default = True
    DefaultCaption = True
    Kind = bkOK
    ModalResult = 1
    OnClick = ButtonClick
    TabOrder = 9
  end
  object btnDelete: TButton
    AnchorSideLeft.Control = lbMounts
    AnchorSideLeft.Side = asrBottom
    AnchorSideTop.Control = btnDown
    AnchorSideTop.Side = asrBottom
    AnchorSideRight.Control = mArcSupported
    AnchorSideRight.Side = asrBottom
    AnchorSideBottom.Side = asrBottom
    Left = 453
    Height = 24
    Top = 119
    Width = 91
    Anchors = [akTop, akLeft, akRight]
    BorderSpacing.Left = 5
    BorderSpacing.Top = 5
    Caption = 'Delete'
    OnClick = ButtonClick
    TabOrder = 4
  end
  object btnAdd: TButton
    AnchorSideLeft.Control = btnDelete
    AnchorSideTop.Control = fneAddFile
    AnchorSideRight.Control = mArcSupported
    AnchorSideRight.Side = asrBottom
    AnchorSideBottom.Control = fneAddFile
    AnchorSideBottom.Side = asrBottom
    Left = 453
    Height = 24
    Top = 32
    Width = 91
    Anchors = [akTop, akLeft, akRight, akBottom]
    Caption = 'Add'
    OnClick = ButtonClick
    TabOrder = 1
  end
  object chkSymlinks: TCheckBox
    AnchorSideLeft.Control = mArcSupported
    AnchorSideTop.Control = lbMounts
    AnchorSideTop.Side = asrBottom
    Left = 16
    Height = 23
    Top = 155
    Width = 249
    BorderSpacing.Top = 10
    Caption = 'Enable following of symbolic links'
    TabOrder = 6
  end
  object lbMounts: TListBox
    AnchorSideLeft.Control = mArcSupported
    AnchorSideTop.Control = fneAddFile
    AnchorSideTop.Side = asrBottom
    AnchorSideRight.Control = fneAddFile
    AnchorSideRight.Side = asrBottom
    Left = 16
    Height = 84
    Top = 61
    Width = 432
    Anchors = [akTop, akLeft, akRight]
    BorderSpacing.Top = 5
    ItemHeight = 0
    ScrollWidth = 430
    TabOrder = 5
    TopIndex = -1
  end
  object btnDown: TButton
    AnchorSideLeft.Control = lbMounts
    AnchorSideLeft.Side = asrBottom
    AnchorSideTop.Control = btnUp
    AnchorSideTop.Side = asrBottom
    AnchorSideRight.Control = mArcSupported
    AnchorSideRight.Side = asrBottom
    AnchorSideBottom.Side = asrBottom
    Left = 453
    Height = 24
    Top = 90
    Width = 91
    Anchors = [akTop, akLeft, akRight]
    BorderSpacing.Left = 5
    BorderSpacing.Top = 5
    Caption = 'Down'
    OnClick = ButtonClick
    TabOrder = 3
  end
  object btnUp: TButton
    AnchorSideLeft.Control = lbMounts
    AnchorSideLeft.Side = asrBottom
    AnchorSideTop.Control = lbMounts
    AnchorSideRight.Control = mArcSupported
    AnchorSideRight.Side = asrBottom
    AnchorSideBottom.Side = asrBottom
    Left = 453
    Height = 24
    Top = 61
    Width = 91
    Anchors = [akTop, akLeft, akRight]
    BorderSpacing.Left = 5
    Caption = 'Up'
    OnClick = ButtonClick
    TabOrder = 2
  end
end
]]


function dialog_proc(Dialog, DlgItemName, Msg, wParam, lParam)
  -- DC.LogWrite(DlgItemName .. string.format(" Msg = 0x%x", Msg), my.lmsgInfo, true, false)
  if (Msg == my.DN_INITDIALOG) then
    DC.LogWrite(DlgItemName .. ' DN_INITDIALOG', my.lmsgInfo, true, false)
    Dialogs.SendDlgMsg(Dialog, nil, my.DM_SETTEXT, 'Monkey', 0)
    Dialogs.SendDlgMsg(Dialog, 'Edit1', my.DM_SETTEXT, 'Caramelldansen', 0)
    Dialogs.SendDlgMsg(Dialog, 'Timer1', my.DM_TIMERSETINTERVAL, 1000, 0)
  elseif (Msg == my.DN_CLICK) then
    DC.LogWrite(DlgItemName .. ' DN_CLICK', my.lmsgInfo, true, false)
    if (DlgItemName == 'Button1') then
      text = Dialogs.SendDlgMsg(Dialog, 'Edit1', my.DM_GETTEXT, 0, 0)
      -- DC.LogWrite(text, my.lmsgInfo, true, false)
      index = Dialogs.SendDlgMsg(Dialog, 'RadioGroup1', my.DM_LISTGETITEMINDEX, 0, 0)
      if index == -1 then
        DC.LogWrite('RadioGroup1 index = '.. index, my.lmsgError, true, false) -- ffff
      else
        text = 'RadioGroup1 = ' .. index .. ', Edit1 = ' .. text
      end
      Dialogs.SendDlgMsg(Dialog, 'ListBox1', my.DM_LISTADD, text, 0)
      Dialogs.SendDlgMsg(Dialog, 'ComboBox1', my.DM_LISTADDSTR, text, 0)
      Dialogs.SendDlgMsg(Dialog, 'SynEdit1', my.DM_LISTADDSTR, text, 0)
      Dialogs.SendDlgMsg(Dialog, 'Memo1', my.DM_LISTADDSTR, text, 0)
      -- Dialogs.SendDlgMsg(Dialog, 'FileNameEdit1', my.DM_SETTEXT, text, 0)
    elseif (DlgItemName == 'MenuItem2') then
      text = Dialogs.SendDlgMsg(Dialog, 'FileNameEdit1', my.DM_GETTEXT, 0, 0)
      Dialogs.SendDlgMsg(Dialog, 'Image1', my.DM_SETTEXT, text, 0)
    elseif (DlgItemName == 'MenuItem3') then
      Dialogs.MessageBox(_VERSION, "", my.MB_OK + my.MB_ICONWARNING)
      
      Dialogs.DialogBoxLFM(lfm_data, false, "dialog_proc")
      
    elseif (DlgItemName == 'Button2') then
      index = Dialogs.SendDlgMsg(Dialog, 'ListBox1', my.DM_LISTGETITEMINDEX, 0, 0)
      if index == -1 then
        DC.LogWrite('ListBox1 index = '.. index, my.lmsgError, true, false) -- ffff
      else
        Dialogs.SendDlgMsg(Dialog, 'ListBox1', my.DM_LISTDELETE, index, 0)
        Dialogs.SendDlgMsg(Dialog, 'ComboBox1', my.DM_LISTDELETE, index, 0)
        Dialogs.SendDlgMsg(Dialog, 'SynEdit1', my.DM_LISTDELETE, index, 0)
        Dialogs.SendDlgMsg(Dialog, 'Memo1', my.DM_LISTDELETE, index, 0)
      end
    end
  elseif (Msg == my.DN_CHANGE) then
    DC.LogWrite(DlgItemName .. ' DN_CHANGE', my.lmsgInfo, true, false)
    if (DlgItemName == 'Edit1') then
      -- DC.LogWrite(Dialogs.ParamToStr(wParam), my.lmsgInfo, true, false)
    elseif (DlgItemName == 'CheckBox1') then
      visible = true
      if (wParam == 1) then 
        visible = false
      end 
      Dialogs.SendDlgMsg(Dialog, 'ListBox1', my.DM_SHOWITEM, visible, 0)
      rect = Dialogs.SendDlgMsg(Dialog, nil, my.DM_GETDLGBOUNDS, 0, 0)
      --DC.LogWrite('Old: Rect.Left = ' .. rect['Left']..' Rect.Right = '.. rect['Right']..' Rect.Top = ' .. rect['Top']..' Rect.Bottom = '.. rect['Bottom'], my.lmsgInfo, true, false)
      rect['Left'] = rect['Left'] - 3
      rect['Right'] = rect['Right'] - 3
      --DC.LogWrite('New: Rect.Left = ' .. rect['Left']..' Rect.Right = '.. rect['Right']..' Rect.Top = ' .. rect['Top']..' Rect.Bottom = '.. rect['Bottom'], my.lmsgInfo, true, false)
      Dialogs.SendDlgMsg(Dialog, nil, my.DM_SETDLGBOUNDS, rect, 0)
    else
      -- DC.LogWrite(wParam, my.lmsgInfo, true, false)
    end
  elseif (Msg == my.DN_DBLCLICK) then
    DC.LogWrite(DlgItemName .. ' DN_DBLCLICK', my.lmsgInfo, true, false)
  elseif (Msg == my.DN_GOTFOCUS) then
    DC.LogWrite(DlgItemName .. ' DN_GOTFOCUS', my.lmsgInfo, true, false)
  elseif (Msg == my.DN_KILLFOCUS) then
    DC.LogWrite(DlgItemName .. ' DN_KILLFOCUS', my.lmsgInfo, true, false)
  elseif (Msg == my.DN_KEYDOWN) then
    DC.LogWrite(DlgItemName .. ' DN_KEYDOWN', my.lmsgInfo, true, false)
    DC.LogWrite(Dialogs.ParamsToKeyStr(wParam, lParam) .. ' : KeyCode = ' .. Dialogs.ParamToKeyCode(wParam) .. ', ShiftState = ' .. lParam, my.lmsgSuccess, true, false)
    if (DlgItemName == 'DirectoryEdit1' and Dialogs.ParamsToKeyStr(wParam, lParam) == 'Shift+`') then
      Dialogs.SendDlgMsg(Dialog, 'DirectoryEdit1', my.DM_SETTEXT, os.getenv('HOME'), 0)
      Dialogs.ParamKeyHandled(wParam)
    end 
  elseif (Msg == my.DN_KEYUP) then
    DC.LogWrite(DlgItemName .. ' DN_KEYUP', my.lmsgInfo, true, false)
  elseif (Msg == my.DN_TIMER) then
    DC.LogWrite(DlgItemName .. ' DN_TIMER', my.lmsgInfo, true, false)
  elseif (Msg == my.DN_CLOSE) then
    DC.LogWrite(DlgItemName .. ' DN_CLOSE', my.lmsgInfo, true, false)
  end

end

DC.LogWrite("VersionAPI "..DC.VersionAPI, my.lmsgInfo, true, false)
Dialogs.DialogBoxLFM(debug.getinfo(1).source:sub(2):match(".*/") .. "dialog.lfm", true, "dialog_proc")
