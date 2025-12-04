local dc = require("doublecmd-common")


monofont_opts = {
  FontName = "FreeMono", FontSize = 8
}


cols = {
  {Title = "name", Data = "[DC().GETFILENAME{}]", Width = 300, Align = "<-"},
  {Title = "size", Data = "[DC().GETFILESIZE{}]", Width = 80, Align = "->", Custom = monofont_opts},
  {Title = "comment", Data = "[DC().GETFILECOMMENT{}]", Width = 300, Align = "<-"},
}

colsets, current, fs = DC.GetColumnSets()

DC.LogWrite(tostring(colsets))

if colsets then
  for i = 1, #colsets do
    DC.LogWrite(colsets[i])
  end
end

if current then
  if fs then
    DC.LogWrite("fs = " ..fs)
  end
  DC.LogWrite("name = " ..current)
else
  DC.LogWrite("FICKUZ", dc.lmsgSuccess)
end


active_panel = true
custom_view = true

DC.SetColumns(cols, active_panel, custom_view)

_, current, fs = DC.GetColumnSets(active_panel)

if current then
  if fs then
    DC.LogWrite("fs = " ..fs)
  end
  DC.LogWrite("name = " ..current)
else
  DC.LogWrite("FICKUZ", dc.lmsgSuccess)
end