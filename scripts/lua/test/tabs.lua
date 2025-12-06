drives = DC.GetDrives()
DC.LogWrite(tostring(drives).. ' #drives= ' ..#drives)
for i= 1, #drives do
  local text = ''
  for key, value in pairs(drives[i]) do
    text = text .. '\tdrives['.. i .. '].' .. tostring(key) .. ' = ' .. tostring(value)
  end
  DC.LogWrite(text)
end


tabs = DC.GetTabsInPanel(is_active)
DC.LogWrite(tostring(tabs).. ' #tabs= ' ..#tabs)
for i= 1, #tabs do
  local text = ''
  for key, value in pairs(tabs[i]) do
    text = text .. '\ttabs['.. i .. '].' .. tostring(key) .. ' = ' .. tostring(value)
  end
  DC.LogWrite(text)
end

