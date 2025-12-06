
is_active = true
is_allfiles = not DC.IsSelectionExists(is_acive)
files = DC.GetFilesInPanel(is_active, is_allfiles)
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


is_paths = true
files = { SysUtils.ExtractFilePath(os.getenv('COMMANDER_EXE')) .. 'scripts/lua/test/flatview.lua'}

DC.LogWrite("DC.MarkFilesInPanel({"..files[1].."}, is_active="..tostring(is_active)..", is_paths="..tostring(is_paths)..") = "..tostring(DC.MarkFilesInPanel(files, is_active, is_paths)))