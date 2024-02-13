
local my = require("doublecmd-common")
os.setlocale("C", "numeric")

local list_first_value = 0
local list_op_value = 1
local list_second_value = 2
local list_calc_mark = 3
local list_result = 4
local list_history = list_result + 2

function list_get_current_item(Dialog)
  local list_item = list_first_value
  local op = Dialogs.SendDlgMsg(Dialog, 'lbCalc', my.DM_LISTGETITEM, list_op_value, 0)
  if op ~= '' then
    list_item = list_second_value
  end
  return list_item
end

function is_calculated(Dialog)
  local calc_mark = Dialogs.SendDlgMsg(Dialog, 'lbCalc', my.DM_LISTGETITEM, list_calc_mark, 0)
  if calc_mark ~= '' then
    return true
  end
  return false
end

function clear_all_input(Dialog)
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTSETITEMINDEX, list_first_value, '0')
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTUPDATE, list_first_value, '0')
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTUPDATE, list_op_value, '')
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTUPDATE, list_second_value, '')
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTUPDATE, list_calc_mark, '')
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTUPDATE, list_result, '')
end

function clear_input(Dialog)
  if is_calculated(Dialog) then
    clear_all_input(Dialog)
  else
    local list_item = list_get_current_item(Dialog)
    Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTSETITEMINDEX, list_item, '0')
    Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTUPDATE, list_item, '0')
  end
end

function digit_backspace(Dialog)
  local current_text = '0'
  local list_item = list_first_value
  if is_calculated(Dialog) then
    current_text = Dialogs.SendDlgMsg(Dialog, 'lbCalc', my.DM_LISTGETITEM, list_result, 0)
    clear_all_input(Dialog)
  else
    list_item = list_get_current_item(Dialog)
    current_text = Dialogs.SendDlgMsg(Dialog, 'lbCalc', my.DM_LISTGETITEM, list_item, 0)
  end
  current_text = current_text:sub(1, -2)
  if current_text == '' then
    current_text = '0'
  end
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTUPDATE, list_item, current_text)
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTSETITEMINDEX, list_item, '0')
end

function digit_update(Dialog, char)
  if is_calculated(Dialog) then
    clear_all_input(Dialog)
  end
  local list_item = list_get_current_item(Dialog)
  local current_text = Dialogs.SendDlgMsg(Dialog, 'lbCalc', my.DM_LISTGETITEM, list_item, 0)
  if current_text:len() == 12 then
    return
  end
  if current_text == '0' and char ~= '.' and char ~= '-' then
    current_text = tostring(char)
  elseif char == '.' and current_text:find("%.") then
    return
  elseif current_text == '0' and char == '-' then
    return
  elseif char == '-' then
    if not current_text:find("^%-") then
      current_text = '-' .. current_text
    else
      current_text = current_text:sub(2, -1)
    end
  else
    current_text = current_text .. char
  end
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTUPDATE, list_item, current_text)
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTSETITEMINDEX, list_item, '0')
end

function set_op(Dialog, op)
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTUPDATE, list_op_value, op)
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTUPDATE, list_second_value, '0')
  if is_calculated(Dialog) then
    local result = Dialogs.SendDlgMsg(Dialog, 'lbCalc', my.DM_LISTGETITEM, list_result, 0)
    Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTUPDATE, list_first_value, result)
    Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTUPDATE, list_calc_mark, '')
    Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTUPDATE, list_result, '')
    Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTSETITEMINDEX, list_first_value, '0')
  end
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTSETITEMINDEX, list_second_value, '0')
end

function calculate(Dialog)
  if is_calculated(Dialog) then
    return
  end
  local first = tonumber(Dialogs.SendDlgMsg(Dialog, 'lbCalc', my.DM_LISTGETITEM, list_first_value, 0))
  local second = tonumber(Dialogs.SendDlgMsg(Dialog, 'lbCalc', my.DM_LISTGETITEM, list_second_value, 0))
  local op = Dialogs.SendDlgMsg(Dialog, 'lbCalc', my.DM_LISTGETITEM, list_op_value, 0)
  local result = 0
  if op == '' then
    return
  elseif op == '+' then
    result = first + second
  elseif op == '-' then
    result = first - second
  elseif op == '*' then
    result = first * second
  elseif op == '/' then
    result = first / second
  elseif op == '%' then
    result = first * second / 100
  end
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTUPDATE, list_calc_mark, '=')
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTUPDATE, list_result, tostring(result))
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTINSERT, list_history, tostring(result))
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTSETITEMINDEX, list_result, '0')
end

function mem_store(Dialog, is_plus)
  local list_item = list_first_value
  if is_calculated(Dialog) then
    list_item = list_result
  else
    list_item = list_get_current_item(Dialog)
  end
  local text = Dialogs.SendDlgMsg(Dialog, 'lbCalc', my.DM_LISTGETITEM, list_item, 0)
  if is_plus == true then
    stored = tonumber(Dialogs.SendDlgMsg(Dialog, 'lblMem', my.DM_GETTEXT, 0, 0))
    text = tostring(tonumber(text) + stored)
  end
  Dialogs.SendDlgMsg(Dialog, 'lblMem', my.DM_SETTEXT, text, 0)
end

function mem_read(Dialog)
  if is_calculated(Dialog) then
    clear_all_input(Dialog)
  end
  local list_item = list_get_current_item(Dialog)
  local text = Dialogs.SendDlgMsg(Dialog, 'lblMem', my.DM_GETTEXT, 0, 0)
  Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTUPDATE, list_item, text)  
end

function dialog_proc(Dialog, DlgItemName, Msg, wParam, lParam)
  if (Msg == my.DN_INITDIALOG) then
    DC.LogWrite(DlgItemName .. ' DN_INITDIALOG', my.lmsgInfo, true, false)

    Dialogs.SendDlgMsg(Dialog, nil, my.DM_SETTEXT, 'Monkey', 0)
    Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTADDSTR, '0', 0)
    for i = 1, 5 do
      Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTADDSTR, '', 0)
    end
  elseif (Msg == my.DN_CLICK) then
    DC.LogWrite(DlgItemName .. ' DN_CLICK', my.lmsgInfo, true, false)

    local digit = DlgItemName:match("btn(%d)")
    if digit ~= nil then
      digit_update(Dialog, digit)
    elseif DlgItemName == 'btnFloat' then
      digit_update(Dialog, '.')
    elseif DlgItemName == 'btnNegative' then
      digit_update(Dialog, '-')
    elseif DlgItemName == 'btnPlus' then
      set_op(Dialog, '+')
    elseif DlgItemName == 'btnMinus' then
      set_op(Dialog, '-')
    elseif DlgItemName == 'btnMult' then
      set_op(Dialog, '*')
    elseif DlgItemName == 'btnDiv' then
      set_op(Dialog, '/')
    elseif DlgItemName == 'btnPrcnt' then
      set_op(Dialog, '%')
    elseif DlgItemName == 'btnMemStore' then
      mem_store(Dialog, false)
    elseif DlgItemName == 'btnMemPlus' then
      mem_store(Dialog, true)
    elseif DlgItemName == 'btnMemRead' then
      mem_read(Dialog)
    elseif DlgItemName == 'btnBkSpc' then
      digit_backspace(Dialog)
    elseif DlgItemName == 'btnClear' then
      clear_input(Dialog)
    elseif DlgItemName == 'btnAllClear' then
      clear_all_input(Dialog)
    elseif DlgItemName == 'btnCalc' then
      calculate(Dialog)
    end
  elseif (Msg == my.DN_CHANGE) then
    DC.LogWrite(DlgItemName .. ' DN_CHANGE', my.lmsgInfo, true, false)
  elseif (Msg == my.DN_DBLCLICK) then
    DC.LogWrite(DlgItemName .. ' DN_DBLCLICK', my.lmsgInfo, true, false)

    if (DlgItemName == 'lbCalc') then
      index = Dialogs.SendDlgMsg(Dialog, 'lbCalc', my.DM_LISTGETITEMINDEX, 0, 0)
      if index >= list_history then
        local list_item = list_first_value
        local calc_mark = Dialogs.SendDlgMsg(Dialog, 'lbCalc', my.DM_LISTGETITEM, list_calc_mark, 0)
        if calc_mark ~= '' then
          clear_all_input(Dialog)
        else
          list_item = list_get_current_item(Dialog)
        end
        local text = Dialogs.SendDlgMsg(Dialog, 'lbCalc', my.DM_LISTGETITEM, index, 0)
        Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTSETITEMINDEX, list_item, '0')
        Dialogs.SendDlgMsg(Dialog, "lbCalc", my.DM_LISTUPDATE, list_item, text)
      end
    end
  elseif (Msg == my.DN_GOTFOCUS) then
    DC.LogWrite(DlgItemName .. ' DN_GOTFOCUS', my.lmsgInfo, true, false)
  elseif (Msg == my.DN_KILLFOCUS) then
    DC.LogWrite(DlgItemName .. ' DN_KILLFOCUS', my.lmsgInfo, true, false)
  elseif (Msg == my.DN_KEYDOWN) then
    DC.LogWrite(DlgItemName .. ' DN_KEYDOWN', my.lmsgInfo, true, false)

    local keycode = tonumber(Dialogs.ParamToKeyCode(wParam))

    DC.LogWrite(Dialogs.ParamsToKeyStr(wParam, lParam) .. ' : KeyCode = ' .. keycode .. ', ShiftState = ' .. lParam, my.lmsgSuccess, true, false)

    if keycode > 95 and keycode < 106 then
      digit_update(Dialog, keycode - 96)
    elseif keycode > 47 and keycode < 58 then
      digit_update(Dialog, keycode - 48)
    elseif keycode == 110 or keycode == 188 or keycode == 190 then
      digit_update(Dialog, '.')
    elseif lParam == 4 and (keycode == 109 or keycode == 189) then
      digit_update(Dialog, '-')
    elseif keycode == 107 or (keycode == 187 and lParam == 1) then
      set_op(Dialog, '+')
    elseif keycode == 189 or keycode == 109 then
      set_op(Dialog, '-')
    elseif keycode == 106 or (keycode == 56 and lParam == 1) then
      set_op(Dialog, '*')
    elseif keycode == 111 or keycode == 191 then
      set_op(Dialog, '/')
    elseif keycode == 53 and lParam == 1 then
      set_op(Dialog, '%')
    elseif lParam == 4 and keycode == 83 then
      mem_store(Dialog, false)
    elseif lParam == 4 and (keycode == 107 or keycode == 187) then
      mem_store(Dialog, true)
    elseif lParam == 4 and keycode == 45 then
      mem_read(Dialog)
    elseif keycode == 8 then
      digit_backspace(Dialog)
    elseif keycode == 46 then
      clear_input(Dialog)
    elseif keycode == 46 and lParam == 1 then
      clear_all_input(Dialog)
    elseif keycode == 13 or keycode == 187 then
      calculate(Dialog)
    end
    Dialogs.ParamKeyHandled(wParam)
  elseif (Msg == my.DN_KEYUP) then
    DC.LogWrite(DlgItemName .. ' DN_KEYUP', my.lmsgInfo, true, false)

    Dialogs.ParamKeyHandled(wParam)
  elseif (Msg == my.DN_TIMER) then
    DC.LogWrite(DlgItemName .. ' DN_TIMER', my.lmsgInfo, true, false)
  elseif (Msg == my.DN_CLOSE) then
    DC.LogWrite(DlgItemName .. ' DN_CLOSE', my.lmsgInfo, true, false)
  end

end

Dialogs.DialogBoxLFM(debug.getinfo(1).source:sub(2):match(".*" .. SysUtils.PathDelim) .. "fcalc.lfm", true, "dialog_proc")
