
local my = require("common")

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
    Font.Name = 'FONT_NAME'
    ItemHeight = 0
    TabOrder = 0
    TopIndex = -1
    OnClick = ListBoxClick
  end
end
]]

local fields = {
{"Target",      "object|object (recursive)", 8},
{"Target (parts)", "name|path|name (recursive)|path (recursive)", 8},
{"Size (x)",    "B|KB|MB|GB", 2},
{"Size (x.y)",  "KB|MB|GB",   3},
{"Size (x.yz)", "KB|MB|GB",   3},
{"Size (auto)", "x|x.y|x.yz", 8}
}


function userdata_dialog_proc(Dialog, DlgItemName, Msg, wParam, lParam, UserData)
  if (Msg == my.DN_INITDIALOG) then
    DC.LogWrite('Userdata type ' .. type(UserData), my.lmsgInfo, true, false)
    for i=1, #UserData do
      Dialogs.SendDlgMsg(Dialog, 'ListBox1', my.DM_LISTADD, UserData[i][1], {UserData[i][2], UserData[i][3]})
    end
    data = Dialogs.SendDlgMsg(Dialog, nil, my.DM_GETDLGDATA)
    DC.LogWrite('Userdata type ' .. type(data), my.lmsgInfo, true, false)
    old = Dialogs.SendDlgMsg(Dialog, nil, my.DM_SETDLGDATA, 123)
    data = Dialogs.SendDlgMsg(Dialog, 'ListBox1', my.DM_GETDLGDATA)
    DC.LogWrite('New userdata type ' .. type(data), my.lmsgInfo, true, false)
    DC.LogWrite('Old userdata type ' .. type(old), my.lmsgInfo, true, false)
  elseif (Msg == my.DN_CLICK) then
    if (DlgItemName == 'ListBox1') then
      index = Dialogs.SendDlgMsg(Dialog, 'ListBox1', my.DM_LISTGETITEMINDEX, 0, 0)
      data =  Dialogs.SendDlgMsg(Dialog, 'ListBox1', my.DM_LISTGETDATA, index, 0)
      DC.LogWrite(data[1] .. ', '.. data[2] .. ' = datatype ' .. type(data), my.lmsgInfo, true, false)
    end
  elseif (Msg == my.DN_CLOSE) then
    Dialogs.SendDlgMsg(Dialog, 'ListBox1', my.DM_LISTCLEAR, 0, 0)
    DC.LogWrite(DlgItemName .. ' DN_CLOSE', my.lmsgInfo, true, false)
  end
end

lfm_data = lfm_data:gsub("FONT_NAME", DC.ConfigGetContent("Fonts/Editor/Name"):gsub("'", "''"))
Dialogs.DialogBoxLFM(lfm_data, false, "userdata_dialog_proc", fields)
