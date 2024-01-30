
local my = require("common")

function test(Dialog, DlgItemName, Msg, wParam, lParam)
  DC.LogWrite(DlgItemName .. " " .. Msg, 1, true, false)
  if (Msg == my.DN_CLICK) then
    if (DlgItemName == 'Button1') then
      text = Dialogs.SendDlgMsg(Dialog, 'Edit1', my.DM_GETTEXT, 0, 0)
      DC.LogWrite(text, 1, true, false)
      Dialogs.SendDlgMsg(Dialog, 'ListBox1', my.DM_LISTADDSTR, text, 0)
      Dialogs.SendDlgMsg(Dialog, 'FileNameEdit1', my.DM_SETTEXT, text, 0)
    elseif (DlgItemName == 'MenuItem1') then
      Dialogs.MessageBox("aaaaaaaaa", "", my.MB_OK + my.MB_ICONWARNING)
    elseif (DlgItemName == 'Button2') then
      index = Dialogs.SendDlgMsg(Dialog, 'ListBox1', my.DM_LISTGETITEMINDEX, 0, 0)
      DC.LogWrite(index, 1, true, false) -- ffff
      Dialogs.SendDlgMsg(Dialog, 'ListBox1', my.DM_LISTDELETE, index, 0)
      Dialogs.SendDlgMsg(Dialog, 'ListBox1', my.DM_SHOWITEM, 0, 0)
    end
  elseif (Msg == my.DN_CHANGE) then
    if (DlgItemName == 'Edit1') then
      DC.LogWrite(Dialogs.ParamToStr(wParam), 1, true, false)
    else
      DC.LogWrite(wParam, 1, true, false)
    end
  end

end

DC.LogWrite("VersionAPI "..DC.VersionAPI, 1, true, false)
Dialogs.DialogBoxLFM(debug.getinfo(1).source:sub(2):match(".*/") .. "dialog.lfm", true, "test")
