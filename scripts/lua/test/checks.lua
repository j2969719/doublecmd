DC.LogWrite('DC.LanguageID "' .. DC.LanguageID .. '"')

is_acive = true

DC.ExecuteCommand("cm_MarkPlus", "mask=*.lfm")
DC.LogWrite('DC.IsSelectionExists ' .. tostring(DC.IsSelectionExists()))
DC.LogWrite('DC.IsInFlatView ' .. tostring(DC.IsInFlatView()))
if DC.IsSelectionExists(is_acive) then
  DC.LogWrite('DC.IsInFlatView ' .. tostring(DC.IsInFlatView()))
  DC.ExecuteCommand("cm_FlatViewSel")
  if DC.IsInFlatView(is_acive) then
    DC.LogWrite('DC.IsInFlatView ' .. tostring(DC.IsInFlatView()))
    DC.ExecuteCommand("cm_FlatViewSel")
  end
  DC.LogWrite('DC.IsInFlatView ' .. tostring(DC.IsInFlatView()))
end

active = DC.ExpandVar("%Ds")
inactive = DC.ExpandVar("%Dt")

DC.ExecuteCommand("cm_ChangeDir", 'activepath=' ..os.getenv("HOME"))
while DC.IsLoadingFileList(is_acive) do --[[ nothing! ]] end
DC.LogWrite('DC.IsSelectionExists ' .. tostring(DC.IsSelectionExists()))
DC.ExecuteCommand("cm_MarkPlus", "mask=*.zip")
if DC.IsSelectionExists(is_acive) then
  DC.LogWrite('DC.IsSelectionExists ' .. tostring(DC.IsSelectionExists()))
  is_acive = false
  DC.ExecuteCommand("cm_ChangeDir", 'inactivepath=/tmp')
  while DC.IsLoadingFileList(is_acive) do --[[ absolutely nothing! ]] end

  is_acive = true
  queueid = 666

  DC.ExecuteCommand("cm_Copy", "confirmation=off", "queueid=" .. queueid)
  DC.LogWrite('DC.IsQueueAssigned ' .. tostring(DC.IsQueueAssigned(queueid))) 
  while DC.IsQueueAssigned(queueid) do --[[ STUPID! you so STU.. ]] end
end
DC.LogWrite('DC.IsQueueAssigned ' .. tostring(DC.IsQueueAssigned(queueid)))
DC.ExecuteCommand("cm_ChangeDir", 'activepath=' ..active)
DC.ExecuteCommand("cm_ChangeDir", 'inactivepath=' ..inactive)
DC.LogWrite("THE END IS NEAR")