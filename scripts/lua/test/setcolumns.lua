local dc = require("doublecmd-common")


monofont_opts = {
  FontName = "FreeMono", FontSize = 8
}


cols = {
  {Title = "name", Data = "[DC().GETFILENAME{}]", Width = 300, Align = "<-"},
  {Title = "size", Data = "[DC().GETFILESIZE{}]", Width = 80, Align = "->", Custom = monofont_opts},
  {Title = "comment", Data = "[DC().GETFILECOMMENT{}]", Width = 300, Align = "<-"},
}

name, fs = DC.GetColumnsInfo()

if name then
  if fs then
    DC.LogWrite("fs = " ..fs)
  end
  DC.LogWrite("name = " ..name)
else
  DC.LogWrite("FICKUZ", dc.lmsgSuccess)
end


active_panel = true
custom_view = true

DC.SetColumns(cols, active_panel, custom_view)

DC.LogWrite("FAIL HERE")
name, fs = DC.GetColumnsInfo()

if name then
  if fs then
    DC.LogWrite("fs = " ..fs, dc.lmsgError)
  end
  DC.LogWrite("name = " ..name, dc.lmsgError)
else
  DC.LogWrite("FICKUZ", dc.lmsgError)
end