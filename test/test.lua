
local my = require("doublecmd-common.lua")

lfm_data = [[
object DialogBox: TDialogBox
  Left = 404
  Height = 296
  Top = 142
  Width = 275
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Sample Text'
  ChildSizing.LeftRightSpacing = 10
  ChildSizing.TopBottomSpacing = 10
  ChildSizing.HorizontalSpacing = 15
  ChildSizing.VerticalSpacing = 15
  ClientHeight = 296
  ClientWidth = 275
  OnClose = DialogBoxClose
  OnShow = DialogBoxShow
  Position = poOwnerFormCenter
  LCLVersion = '3.0.0.3'
  object ListBox1: TListBox
    Left = 10
    Height = 272
    Top = 10
    Width = 254
    ItemHeight = 0
    TabOrder = 0
    TopIndex = -1
    OnClick = ListBoxClick
  end
end
]]


function dialog_proc(Dialog, DlgItemName, Msg, wParam, lParam)
  -- DC.LogWrite(DlgItemName .. string.format(" Msg = 0x%x", Msg), my.lmsgInfo, true, false)
  if (Msg == my.DN_INITDIALOG) then
    DC.LogWrite(DlgItemName .. ' DN_INITDIALOG', my.lmsgInfo, true, false)
    Dialogs.SendDlgMsg(Dialog, nil, my.DM_SETTEXT, 'Monkey', 0)
    Dialogs.SendDlgMsg(Dialog, 'Edit1', my.DM_SETTEXT, 'Caramelldansen', 0)
    Dialogs.SendDlgMsg(Dialog, 'Timer1', my.DM_TIMERSETINTERVAL, 0, 0)
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
      Dialogs.SendDlgMsg(Dialog, 'ListBox1', my.DM_LISTADDSTR, text, 0)
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
    elseif (DlgItemName == 'SpinEditEx1') then
      text = Dialogs.SendDlgMsg(Dialog, 'SpinEditEx1', my.DM_GETTEXT, 0, 0)
      Dialogs.SendDlgMsg(Dialog, 'ProgressBar1', my.DM_SETPROGRESSVALUE, tonumber(text), 0)
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
