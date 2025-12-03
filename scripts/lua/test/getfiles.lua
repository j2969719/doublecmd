
is_active = true
is_allfiles = not DC.IsSelectionExists(is_acive)
files = DC.FilesInPanel(is_active, is_allfiles)
DC.LogWrite(tostring(files).. ' #files= ' ..#files )
for i= 1, #files do
  local text = ''
  for key, value in pairs(files[i]) do
    text = text .. '\tfiles['.. i .. '].' .. tostring(key) .. ' = ' .. tostring(value)
  end
  DC.LogWrite(text)
  --DC.LogWrite(files[i].Name .. ' -> "' .. tostring(files[i].LinkTo) .. '"" ' .. os.date("%c", files[i].ModificationTime))
end

files = { "getfiles.lua", "checks.lua" }

DC.LogWrite("DC.MarkFilesInPanel(files, is_active) = "..tostring(DC.MarkFilesInPanel(files, is_active)))