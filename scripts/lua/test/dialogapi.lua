local my = require("doublecmd-common")

lfm_data = [[
object DialogBox: TDialogBox
  Left = 404
  Height = 391
  Top = 142
  Width = 548
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Sample Text'
  ChildSizing.LeftRightSpacing = 10
  ChildSizing.TopBottomSpacing = 10
  ClientHeight = 391
  ClientWidth = 548
  OnClose = DialogBoxClose
  OnShow = DialogBoxShow
  Position = poOwnerFormCenter
  LCLVersion = '3.4.0.0'
  object gbProps: TGroupBox
    Left = 10
    Height = 132
    Top = 10
    Width = 528
    Align = alTop
    AutoSize = True
    Caption = 'gbProps'
    ChildSizing.LeftRightSpacing = 5
    ChildSizing.TopBottomSpacing = 5
    ChildSizing.HorizontalSpacing = 5
    ChildSizing.VerticalSpacing = 5
    ClientHeight = 115
    ClientWidth = 526
    TabOrder = 0
    object rgType: TRadioGroup
      AnchorSideLeft.Control = gbProps
      AnchorSideTop.Control = gbProps
      Left = 5
      Height = 105
      Top = 5
      Width = 185
      AutoFill = True
      Caption = 'rgType'
      ChildSizing.LeftRightSpacing = 6
      ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
      ChildSizing.EnlargeVertical = crsHomogenousChildResize
      ChildSizing.ShrinkHorizontal = crsScaleChilds
      ChildSizing.ShrinkVertical = crsScaleChilds
      ChildSizing.Layout = cclLeftToRightThenTopToBottom
      ChildSizing.ControlsPerLine = 1
      ClientHeight = 88
      ClientWidth = 183
      Items.Strings = (
        'string'
        'float'
        'int32'
        'int64'
        'boolean'
      )
      TabOrder = 0
    end
    object edComponent: TEdit
      AnchorSideLeft.Control = lblComponent
      AnchorSideTop.Control = lblComponent
      AnchorSideTop.Side = asrBottom
      AnchorSideRight.Control = mPropValue
      Left = 195
      Height = 28
      Top = 26
      Width = 171
      Anchors = [akTop, akLeft, akRight]
      BorderSpacing.Top = 5
      TabOrder = 1
      Text = 'edComponent'
      OnChange = EditChange
      OnClick = EditClick
      OnDblClick = EditDblClick
      OnEnter = EditEnter
      OnExit = EditExit
      OnKeyDown = EditKeyDown
      OnKeyUp = EditKeyUp
    end
    object lblComponent: TLabel
      AnchorSideLeft.Control = rgType
      AnchorSideLeft.Side = asrBottom
      AnchorSideTop.Control = gbProps
      Left = 195
      Height = 16
      Top = 5
      Width = 80
      Caption = 'lblComponent'
    end
    object edPropName: TEdit
      AnchorSideLeft.Control = lblPropName
      AnchorSideTop.Control = lblPropName
      AnchorSideTop.Side = asrBottom
      AnchorSideRight.Control = edComponent
      AnchorSideRight.Side = asrBottom
      Left = 195
      Height = 28
      Top = 80
      Width = 171
      Anchors = [akTop, akLeft, akRight]
      BorderSpacing.Top = 5
      TabOrder = 2
      Text = 'Caption'
      OnChange = EditChange
      OnClick = EditClick
      OnDblClick = EditDblClick
      OnEnter = EditEnter
      OnExit = EditExit
      OnKeyDown = EditKeyDown
      OnKeyUp = EditKeyUp
    end
    object lblPropName: TLabel
      AnchorSideLeft.Control = lblComponent
      AnchorSideTop.Control = edComponent
      AnchorSideTop.Side = asrBottom
      Left = 195
      Height = 16
      Top = 59
      Width = 75
      BorderSpacing.Top = 5
      Caption = 'lblPropName'
    end
    object btnGet: TButton
      AnchorSideLeft.Control = mPropValue
      AnchorSideTop.Control = mPropValue
      AnchorSideTop.Side = asrBottom
      AnchorSideBottom.Control = edPropName
      AnchorSideBottom.Side = asrBottom
      Left = 371
      Height = 28
      Top = 80
      Width = 75
      Anchors = [akTop, akLeft, akBottom]
      BorderSpacing.Top = 5
      Caption = 'btnGet'
      TabOrder = 3
      OnClick = ButtonClick
      OnEnter = ButtonEnter
      OnExit = ButtonExit
      OnKeyDown = ButtonKeyDown
      OnKeyUp = ButtonKeyUp
    end
    object mPropValue: TMemo
      AnchorSideTop.Control = gbProps
      AnchorSideRight.Control = gbProps
      AnchorSideRight.Side = asrBottom
      AnchorSideBottom.Control = lblPropName
      AnchorSideBottom.Side = asrBottom
      Left = 371
      Height = 70
      Top = 5
      Width = 150
      Anchors = [akTop, akRight, akBottom]
      Lines.Strings = (
        'mPropValue'
      )
      TabOrder = 4
    end
    object btnSet: TButton
      AnchorSideLeft.Control = btnGet
      AnchorSideLeft.Side = asrBottom
      AnchorSideTop.Control = btnGet
      AnchorSideRight.Control = gbProps
      AnchorSideRight.Side = asrBottom
      AnchorSideBottom.Control = btnGet
      AnchorSideBottom.Side = asrBottom
      Left = 451
      Height = 28
      Top = 80
      Width = 70
      Anchors = [akTop, akLeft, akRight, akBottom]
      Caption = 'btnSet'
      TabOrder = 5
      OnClick = ButtonClick
      OnEnter = ButtonEnter
      OnExit = ButtonExit
      OnKeyDown = ButtonKeyDown
      OnKeyUp = ButtonKeyUp
    end
  end
  object gbCreate: TGroupBox
    Left = 10
    Height = 121
    Top = 142
    Width = 528
    Align = alTop
    AutoSize = True
    Caption = 'gbCreate'
    ChildSizing.LeftRightSpacing = 5
    ChildSizing.TopBottomSpacing = 5
    ChildSizing.HorizontalSpacing = 5
    ChildSizing.VerticalSpacing = 5
    ClientHeight = 104
    ClientWidth = 526
    TabOrder = 1
    object lblParent: TLabel
      AnchorSideLeft.Control = gbCreate
      AnchorSideTop.Control = gbCreate
      Left = 5
      Height = 16
      Top = 5
      Width = 51
      Caption = 'lblParent'
    end
    object edParent: TEdit
      AnchorSideLeft.Control = lblParent
      AnchorSideTop.Control = lblParent
      AnchorSideTop.Side = asrBottom
      Left = 5
      Height = 28
      Top = 26
      Width = 139
      TabOrder = 0
      Text = 'pnlTest'
      OnChange = EditChange
      OnClick = EditClick
      OnDblClick = EditDblClick
      OnEnter = EditEnter
      OnExit = EditExit
      OnKeyDown = EditKeyDown
      OnKeyUp = EditKeyUp
    end
    object lblName: TLabel
      AnchorSideLeft.Control = edParent
      AnchorSideLeft.Side = asrBottom
      AnchorSideTop.Control = gbCreate
      Left = 149
      Height = 16
      Top = 5
      Width = 48
      Caption = 'lblName'
    end
    object edName: TEdit
      AnchorSideLeft.Control = lblName
      AnchorSideTop.Control = lblName
      AnchorSideTop.Side = asrBottom
      Left = 149
      Height = 28
      Top = 26
      Width = 155
      TabOrder = 1
      Text = 'edName'
      OnChange = EditChange
      OnClick = EditClick
      OnDblClick = EditDblClick
      OnEnter = EditEnter
      OnExit = EditExit
      OnKeyDown = EditKeyDown
      OnKeyUp = EditKeyUp
    end
    object lblClass: TLabel
      AnchorSideLeft.Control = edName
      AnchorSideLeft.Side = asrBottom
      AnchorSideTop.Control = gbCreate
      Left = 309
      Height = 16
      Top = 5
      Width = 47
      Caption = 'lblClass'
    end
    object cbClass: TComboBox
      AnchorSideLeft.Control = lblClass
      AnchorSideTop.Control = lblClass
      AnchorSideTop.Side = asrBottom
      Left = 309
      Height = 28
      Top = 26
      Width = 100
      ItemHeight = 0
      Items.Strings = (
        'TTimer'
        'TButton'
        'TBitBtn'
        'TFileNameEdit'
        'TDirectoryEdit'
        'TComboBox'
        'TListBox'
        'TCheckBox'
        'TGroupBox'
        'TLabel'
        'TPanel'
        'TEdit'
        'TMemo'
        'TImage'
        'TTabSheet'
        'TScrollBox'
        'TRadioGroup'
        'TPageControl'
        'TProgressBar'
        'TDividerBevel'
        'TSynEdit '
      )
      Style = csDropDownList
      TabOrder = 2
      OnChange = ComboBoxChange
      OnClick = ComboBoxClick
      OnDblClick = ComboBoxDblClick
      OnEnter = ComboBoxEnter
      OnExit = ComboBoxExit
      OnKeyDown = ComboBoxKeyDown
      OnKeyUp = ComboBoxKeyUp
    end
    object btnCreate: TButton
      AnchorSideLeft.Control = cbClass
      AnchorSideLeft.Side = asrBottom
      AnchorSideTop.Control = cbClass
      AnchorSideBottom.Control = cbClass
      AnchorSideBottom.Side = asrBottom
      Left = 414
      Height = 28
      Top = 26
      Width = 75
      Anchors = [akTop, akLeft, akBottom]
      Caption = 'btnCreate'
      TabOrder = 3
      OnClick = ButtonClick
      OnEnter = ButtonEnter
      OnExit = ButtonExit
      OnKeyDown = ButtonKeyDown
      OnKeyUp = ButtonKeyUp
    end
    object pnlTest: TPanel
      AnchorSideLeft.Control = gbCreate
      AnchorSideTop.Control = btnCreate
      AnchorSideTop.Side = asrBottom
      AnchorSideRight.Control = gbCreate
      AnchorSideRight.Side = asrBottom
      Left = 5
      Height = 40
      Top = 59
      Width = 516
      Anchors = [akTop, akLeft, akRight]
      AutoSize = True
      Caption = 'pnlTest'
      ChildSizing.LeftRightSpacing = 5
      ChildSizing.TopBottomSpacing = 5
      ChildSizing.HorizontalSpacing = 5
      ChildSizing.VerticalSpacing = 5
      ChildSizing.EnlargeHorizontal = crsHomogenousChildResize
      ChildSizing.Layout = cclLeftToRightThenTopToBottom
      ChildSizing.ControlsPerLine = 2
      Constraints.MinHeight = 40
      TabOrder = 4
    end
  end
end
]]

-- dialog func
function dialog_proc(Dialog, DlgItemName, Msg, wParam, lParam)
  if (Msg == my.DN_INITDIALOG) then
    -- init notify
    DC.LogWrite(DlgItemName .. ' DN_INITDIALOG', my.lmsgInfo, true, false)
    -- clean text
    Dialogs.SendDlgMsg(Dialog, 'mPropValue', my.DM_SETTEXT, '')
    -- set radiogroup to string
    Dialogs.SendDlgMsg(Dialog, 'rgType', my.DM_LISTSETITEMINDEX, 0)
    -- set combobox to tbutton
    Dialogs.SendDlgMsg(Dialog, 'cbClass', my.DM_LISTSETITEMINDEX, 1)
    -- set test component name
    Dialogs.SendDlgMsg(Dialog, 'edComponent', my.DM_SETTEXT, 'MagickButton')
    Dialogs.SendDlgMsg(Dialog, 'edName', my.DM_SETTEXT, 'MagickButton')
  elseif (Msg == my.DN_CLICK) then
    -- click notify
    DC.LogWrite(DlgItemName .. ' DN_CLICK', my.lmsgInfo, true, false)
    if (DlgItemName == 'btnCreate') then
      local parent = Dialogs.SendDlgMsg(Dialog, 'edParent', my.DM_GETTEXT)
      if parent == '' then parent = nil end -- use dialog itself instead child component
      local component = Dialogs.SendDlgMsg(Dialog, 'edName', my.DM_GETTEXT)
      local class = Dialogs.SendDlgMsg(Dialog, 'cbClass', my.DM_GETTEXT)
      if not Dialogs.CreateComponent(Dialog, parent, component, class) then
         Dialogs.MessageBox("failed to create " .. component, "nope", my.MB_OK + my.MB_ICONWARNING)
      end
    else
      local index = Dialogs.SendDlgMsg(Dialog, 'rgType', my.DM_LISTGETITEMINDEX, 0, 0)
      local prop_type = index + 1
      local component = Dialogs.SendDlgMsg(Dialog, 'edComponent', my.DM_GETTEXT)
      if component == '' then component = nil end -- use dialog itself instead child component
      local prop = Dialogs.SendDlgMsg(Dialog, 'edPropName', my.DM_GETTEXT, 0, 0)
      if (DlgItemName == 'btnGet') then
        local value = Dialogs.GetProperty(Dialog, component, prop, prop_type)
        Dialogs.SendDlgMsg(Dialog, 'mPropValue', my.DM_SETTEXT, tostring(value))
      elseif (DlgItemName == 'btnSet') then
        local value = Dialogs.SendDlgMsg(Dialog, 'mPropValue', my.DM_GETTEXT)
        if index > 0 and index < 4 then
          value = tonumber(value)
        elseif index == 4 then
          if value == '0' or value == 'false' then
            value = false
          else
            value = true
          end
        end
        if not Dialogs.SetProperty(Dialog, component, prop, value, prop_type) then
           Dialogs.MessageBox("failed to set prop " .. prop, "nope", my.MB_OK + my.MB_ICONWARNING)
        end
      end
    end

  -- etc
  elseif (Msg == my.DN_CHANGE) then
    DC.LogWrite(DlgItemName .. ' DN_CHANGE', my.lmsgInfo, true, false)
  elseif (Msg == my.DN_DBLCLICK) then
    DC.LogWrite(DlgItemName .. ' DN_DBLCLICK', my.lmsgInfo, true, false)
  elseif (Msg == my.DN_GOTFOCUS) then
    DC.LogWrite(DlgItemName .. ' DN_GOTFOCUS', my.lmsgInfo, true, false)
  elseif (Msg == my.DN_KILLFOCUS) then
    DC.LogWrite(DlgItemName .. ' DN_KILLFOCUS', my.lmsgInfo, true, false)
  elseif (Msg == my.DN_KEYDOWN) then
    DC.LogWrite(DlgItemName .. ' DN_KEYDOWN', my.lmsgInfo, true, false)
    -- log keypress
    DC.LogWrite(Dialogs.ParamsToKeyStr(wParam, lParam) .. ' : KeyCode = ' .. Dialogs.ParamToKeyCode(wParam) .. ', ShiftState = ' .. lParam, my.lmsgSuccess, true, false)
  elseif (Msg == my.DN_KEYUP) then
    DC.LogWrite(DlgItemName .. ' DN_KEYUP', my.lmsgInfo, true, false)
  elseif (Msg == my.DN_TIMER) then
    DC.LogWrite(DlgItemName .. ' DN_TIMER', my.lmsgInfo, true, false)
  elseif (Msg == my.DN_CLOSE) then
    DC.LogWrite(DlgItemName .. ' DN_CLOSE', my.lmsgInfo, true, false)
  end
end

DC.LogWrite("LuaAPI "..DC.LuaAPI.." ExtensionAPI "..DC.ExtensionAPI, my.lmsgInfo, true, false)
local is_lfm_filename = false
-- DialogBoxLFM: lfm, is_file, func_name
Dialogs.DialogBoxLFM(lfm_data, is_lfm_filename, "dialog_proc")

