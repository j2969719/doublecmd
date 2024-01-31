local define =
{
  DM_CLOSE = 1, -- A signal that the dialog is about to close
  DM_ENABLE = 2,
  DM_GETDLGDATA = 3,
  DM_GETDLGBOUNDS = 4,
  DM_GETITEMBOUNDS = 5,
  DM_GETTEXT = 6, -- Retrieve the text of an edit string or the caption of an item
  DM_KEYDOWN = 7,
  DM_KEYUP = 8,
  DM_SETDLGDATA = 9,
  DM_SETFOCUS = 10, -- Set the keyboard focus to the given dialog item
  DM_REDRAW = 11, -- Redraw the whole dialog
  DM_SETTEXT = 12, -- Set a new string value for an edit line or a new caption for an item
  DM_SETMAXTEXTLENGTH = 13, -- Set the maximum length of an edit string
  DM_SHOWDIALOG = 14, -- Show/hide the dialog window
  DM_SHOWITEM = 15, -- Show/hide a dialog item
  DM_GETCHECK = 16, -- Retrieve the state of TCheckBox or TRadioButton items
  DM_SETCHECK = 17, -- Change the state of TCheckBox and TRadioButton items
  DM_LISTGETITEM = 18, -- Retrieve a list item
  DM_LISTGETITEMINDEX = 19, -- Get current item index in a list
  DM_LISTSETITEMINDEX = 20, -- Set current item index in a list
  DM_LISTDELETE = 21,
  DM_LISTADD = 22,
  DM_LISTADDSTR = 23,
  DM_LISTUPDATE = 24,
  DM_LISTINSERT = 25,
  DM_LISTINDEXOF = 26,
  DM_LISTGETCOUNT = 27,
  DM_LISTGETDATA = 28,
  DM_LISTSETDATA = 29,
  DM_SETDLGBOUNDS = 30,
  DM_SETITEMBOUNDS = 31,
  DM_GETDROPPEDDOWN = 32,
  DM_SETDROPPEDDOWN = 33,
  DM_GETITEMDATA = 34,
  DM_SETITEMDATA = 35,
  DM_LISTSET = 36,
  DM_SETPROGRESSVALUE = 37,
  DM_SETPROGRESSSTYLE = 38,
  DM_SETPASSWORDCHAR = 39,
  DM_LISTCLEAR = 40,
  DM_TIMERSETINTERVAL = 41,

  -- events messages
  DN_CLICK = 0x1001, -- Sent after mouse click
  DN_DBLCLICK = 0x1002, -- Sent after mouse double click
  DN_CHANGE = 0x1003, -- Sent after the dialog item is changed
  DN_GOTFOCUS = 0x1004, -- Sent when the dialog item gets input focus
  DN_INITDIALOG = 0x1005, -- Sent before showing the dialog
  DN_KILLFOCUS = 0x1006, -- Sent before a dialog item loses the input focus
  DN_TIMER = 0x1007, -- Sent when a timer expires

  DN_KEYDOWN = 7,
  DN_KEYUP = 8,
  DN_CLOSE = 1, -- Sent before the dialog is closed

  DM_USER = 0x4000, -- Starting value for user defined messages

  -- MessageBox: To indicate the buttons displayed in the message box,
  -- specify one of the following values.
  MB_OK = 0x00000000,
  MB_OKCANCEL = 0x00000001,
  MB_ABORTRETRYIGNORE = 0x00000002,
  MB_YESNOCANCEL = 0x00000003,
  MB_YESNO = 0x00000004,
  MB_RETRYCANCEL = 0x00000005,
  MB_ICONHAND = 0x00000010,
  MB_ICONQUESTION = 0x00000020,
  MB_ICONEXCLAMATION = 0x00000030,
  MB_ICONASTERICK = 0x00000040,
  MB_ICONWARNING = 0x00000030,
  MB_ICONERROR = 0x00000010,
  MB_ICONSTOP = 0x00000010,
  MB_ICONINFORMATION = 0x00000040,
  -- MessageBox: To indicate the default button, specify one of the following values.
  MB_DEFBUTTON1 = 0x00000000,
  MB_DEFBUTTON2 = 0x00000100,
  MB_DEFBUTTON3 = 0x00000200,
  MB_DEFBUTTON4 = 0x00000300,
  -- MessageBox: Return values
  ID_OK = 1,
  ID_CANCEL = 2,
  ID_ABORT = 3,
  ID_RETRY = 4,
  ID_IGNORE = 5,
  ID_YES = 6,
  ID_NO = 7,
  ID_CLOSE = 8,
  ID_HELP = 9,

  -- LogWrite
  lmsgInfo = 0,
  lmsgSuccess = 1,
  lmsgError = 2,
  -- Active panel
  apLeft = 0,
  apRight = 1,

  -- Attr
  faReadOnly = 0x00000001, -- The file is read-only.
  faHidden = 0x00000002, -- The file is hidden. In Unix/Linux, this means that the filename starts with a dot.
  faSysFile = 0x00000004, -- The file is a system file. In Unix/Linux, this means that the file is a character, block or FIFO file.
  faVolumeId = 0x00000008, -- Volume Label. Only for DOS/Windows on a plain FAT (not VFAT or FAT32) filesystem.
  faDirectory = 0x00000010, -- File is a directory.
  faArchive = 0x00000020, -- File is archived. Not possible in Unix/Linux.
  faSymLink = 0x00000400, -- File is a symbolic link.

  -- WDX

  ft_nomorefields = 0,
  ft_numeric_32, 1,
  ft_numeric_64, 2,
  ft_numeric_floating = 3,
  ft_date = 4,
  ft_time = 5,
  ft_boolean = 6,
  ft_multiplechoice = 7,
  ft_string = 8,
  ft_fulltext = 9,
  ft_datetime = 10,
  ft_stringw = 11,
  ft_fulltextw = 12,

  ft_comparecontent = 100,

  -- for ContentGetValue
  ft_nosuchfield = -1, -- error, invalid field number given
  ft_fileerror = -2, -- file i/o error
  ft_fieldempty = -3, -- field valid, but empty
  ft_ondemand = -4, -- field will be retrieved only when user presses <SPACEBAR>
  ft_notsupported = -5, -- function not supported
  ft_setcancel = -6, -- user clicked cancel in field editor
  ft_delayed = 0, -- field takes a long time to extract = -> try again in background
  -- for ContentFindValue:
  ft_found = 1, -- Value was found
  ft_notfound = -8, -- Value was NOT found

  -- for ContentSetValue
  ft_setsuccess = 0, -- setting of the attribute succeeded

  -- for ContentGetSupportedFieldFlags
  contflags_edit = 1,
  contflags_substsize = 2,
  contflags_substdatetime = 4,
  contflags_substdate = 6,
  contflags_substtime = 8,
  contflags_substattributes = 10,
  contflags_substattributestr = 12,
  contflags_passthrough_size_float = 14,
  contflags_substmask = 14,
  contflags_fieldedit = 16,
  contflags_fieldsearch = 32,
  contflags_searchpageonly = 64,

  contst_readnewdir = 1,
  contst_refreshpressed = 2,
  contst_showhint = 4,

  setflags_first_attribute = 1, -- First attribute of this file
  setflags_last_attribute = 2, -- Last attribute of this file
  setflags_only_date = 4, -- Only set the date of the datetime value!

  editflags_initialize = 1, -- The data passed to the plugin may be used to initialize the edit dialog

  CONTENT_DELAYIFSLOW = 1, -- ContentGetValue called in foreground
  CONTENT_PASSTHROUGH = 2, -- If requested via contflags_passthrough_size_float: The size is passed in as floating value, TC expects correct value m from the given units value, and optionally a text string

}

return define
