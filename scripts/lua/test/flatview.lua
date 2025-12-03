is_active = true
is_in_fs_prop = true
if DC.MarkFilesInPanel({"flatview.lua"}) then
  DC.ExecuteCommand("cm_FlatViewSel")
  DC.LogWrite('DC.IsInFlatView(is_active) ' .. tostring(DC.IsInFlatView()))
  DC.LogWrite('DC.IsInFlatView(is_active, is_in_fs_prop) ' .. tostring(DC.IsInFlatView(is_active, is_in_fs_prop)))
  if DC.IsInFlatView(is_active, is_in_fs_prop) then
    DC.ExecuteCommand("cm_FlatView")
  elseif DC.IsInFlatView(is_active) then
    DC.ExecuteCommand("cm_FlatViewSel")
  end
end
