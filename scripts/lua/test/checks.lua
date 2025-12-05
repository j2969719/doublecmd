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

DC.ExecuteCommand("cm_ChangeDir", 'activepath=/usr/bin')
start = SysUtils.GetTickCount()
while DC.IsLoadingFileList() do
  if SysUtils.GetTickCount() - start == 1000 then
    DC.ExecuteCommand("cm_AbortListLoad")
    DC.LogWrite('/usr/bin: DC.ExecuteCommand("cm_AbortListLoad")')
  end
end

DC.ExecuteCommand("cm_ChangeDir", 'activepath=' ..os.getenv("HOME"))
while DC.IsLoadingFileList(is_acive) do --[[ nothing! ]] end
DC.LogWrite('DC.IsSelectionExists ' .. tostring(DC.IsSelectionExists()))
DC.ExecuteCommand("cm_MarkPlus", "mask=*.zip")
if DC.IsSelectionExists(is_acive) then
  DC.LogWrite('DC.IsSelectionExists ' .. tostring(DC.IsSelectionExists()))
  is_acive = false
  newdir = '/tmp/someshit'
  SysUtils.CreateDirectory(newdir)
  DC.ExecuteCommand("cm_ChangeDir", 'inactivepath='..newdir)
  while DC.IsLoadingFileList(is_acive) do --[[ absolutely nothing! ]] end
  DC.LogWrite(DC.ExpandVar("source = %Ds\ttarhet=%Dt"))
  if DC.ExpandVar("%Dt") ~= newdir then
    DC.LogWrite('target is not '.. newdir)
    DC.ExecuteCommand("cm_ChangeDir", 'activepath=' ..active)
    DC.ExecuteCommand("cm_ChangeDir", 'inactivepath=' ..inactive)
    return
  end

  is_acive = true
  queueid = 666

  DC.LogWrite('DC.IsQueueAssigned ' .. tostring(DC.IsQueueAssigned(queueid)))
  DC.ExecuteCommand("cm_Copy", "confirmation=off", "queueid=" .. queueid)
  DC.LogWrite('DC.IsQueueAssigned ' .. tostring(DC.IsQueueAssigned(queueid)))
  while DC.IsQueueAssigned(queueid) do
    if DC.IsQueuePaused(queueid) then
      DC.LogWrite('DC.IsQueuePaused ' .. tostring(DC.IsQueuePaused(queueid)) .. ' UNPAUSE')
      DC.PauseQueue(queueid, false)
      DC.LogWrite('DC.IsQueuePaused ' .. tostring(DC.IsQueuePaused(queueid)) .. ' STOP')
      DC.StopQueue(queueid)
    end
  end
end
DC.LogWrite('DC.IsQueueAssigned ' .. tostring(DC.IsQueueAssigned(queueid)))
DC.ExecuteCommand("cm_ChangeDir", 'activepath=' ..active)
DC.ExecuteCommand("cm_ChangeDir", 'inactivepath=' ..inactive)
DC.LogWrite("THE END IS NEAR")