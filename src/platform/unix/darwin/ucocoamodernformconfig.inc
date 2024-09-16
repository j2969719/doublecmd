{%MainUnit ucocoamodernformconfig.pas}

{$mode ObjFPC}{$H+}
{$modeswitch objectivec2}

interface

uses
  Classes, SysUtils,
  LCLType, Forms, Menus,
  fMain, uMyDarwin, uDCUtils, uLng,
  uFileView, uBriefFileView, uColumnsFileView, uThumbFileView,
  CocoaAll, CocoaConfig, CocoaToolBar, Cocoa_Extra;

procedure initCocoaModernFormConfig;

implementation

procedure toggleTreeViewAction( const Sender: id );
begin
  frmMain.Commands.cm_TreeView([]);
end;

procedure toggleHorzSplitAction( const Sender: id );
begin
  frmMain.Commands.cm_HorizontalFilePanels([]);
end;

procedure swapPanelsAction( const Sender: id );
begin
  frmMain.Commands.cm_Exchange([]);
end;

procedure showModeAction( const Sender: id );
var
  showModeItem: NSToolBarItemGroup absolute Sender;
begin
  case showModeItem.selectedIndex of
    0: frmMain.Commands.cm_BriefView([]);
    1: frmMain.Commands.cm_ColumnsView([]);
    2: frmMain.Commands.cm_ThumbnailsView([]);
  end;
end;

procedure onFileViewUpdated( const fileView: TFileView );
var
  itemGroup: NSToolbarItemGroup;
  itemGroupWrapper: TCocoaToolBarItemGroupWrapper;
begin
  itemGroup:= NSToolbarItemGroup( TCocoaToolBarUtils.findItemByIdentifier( frmMain, 'MainForm.ShowMode' ) );
  if NOT Assigned(itemGroup) then
    Exit;
  itemGroupWrapper:= TCocoaToolBarItemGroupWrapper( itemGroup.target );

  if fileView is TColumnsFileView then
    itemGroupWrapper.lclSetSelectedIndex( 1 )
  else if fileView is TBriefFileView then
    itemGroupWrapper.lclSetSelectedIndex( 0 )
  else if fileView is TThumbFileView then
    itemGroupWrapper.lclSetSelectedIndex( 2 );
end;

function onGetSharingItems( item: NSToolBarItem ): TStringArray;
begin
  Result:= frmMain.NSServiceMenuGetFilenames();
end;

procedure airdropAction( const Sender: id );
begin
  showMacOSAirDropDialog;
end;

procedure quickLookAction( const Sender: id );
begin
  showQuickLookPanel;
end;

procedure finderRevealAction( const Sender: id );
begin
  performMacOSService( 'Finder/Reveal' );
end;

procedure finderInfoAction( const Sender: id );
begin
  performMacOSService( 'Finder/Show Info' );
end;



const
  COMMON_FOLDERS: TStringArray = (
    '~/Documents',
    '~/Desktop',
    '~',
    '~/Pictures',
    '~/Movies',
    '~/Music',
    '/Applications/Utilities',

    '~/Downloads',
    '~/Library',
    '/Applications',

    '~/.Trash'
  );

type
  
  { TToolBarMenuHandler }

  TToolBarMenuHandler = class
  public
    procedure showHotlist( Sender: TObject );
    procedure showFavoriteTabs( Sender: TObject );
    procedure showQuickLookPanel( Sender: TObject );
    procedure goToFolder( Sender: TObject );
  end;

var
  toolBarMenuHandler: TToolBarMenuHandler;

procedure TToolBarMenuHandler.showHotlist( Sender: TObject );
begin
  frmMain.Commands.cm_DirHotList(['position=cursor']);
end;

procedure TToolBarMenuHandler.showFavoriteTabs( Sender: TObject );
begin
  frmMain.Commands.cm_LoadFavoriteTabs(['position=cursor']);
end;

procedure TToolBarMenuHandler.showQuickLookPanel( Sender: TObject );
begin
  quickLookAction( nil );
end;

procedure TToolBarMenuHandler.goToFolder(Sender: TObject);
var
  menuItem: TMenuItem absolute Sender;
  path: String;
begin
  path:= uDCUtils.ReplaceTilde( COMMON_FOLDERS[menuItem.Tag] );
  frmMain.Commands.cm_ChangeDir( [path] );
end;

function getMacOSDisplayNameFromPath(const path: String): String;
var
  cocoaPath: NSString;
  displayName: NSString;
begin
  cocoaPath:= StringToNSString( uDCUtils.ReplaceTilde(path) );
  displayName:= NSFileManager.defaultManager.displayNameAtPath( cocoaPath );
  Result:= displayName.UTF8String;
end;

function onGetFolderMenu: TMenuItem;
var
  menu: TMenuItem;
  tag: PtrInt = 0;

  function newItem( caption: String ): TMenuItem;
  var
    folderName: String;
  begin
    folderName:= getMacOSDisplayNameFromPath( COMMON_FOLDERS[tag] );
    Result:= TMenuItem.Create( menu );
    Result.Caption:= caption + '  ' + folderName;
    Result.onClick:= @toolBarMenuHandler.goToFolder;
    Result.Tag:= tag;
    inc( tag );
  end;

begin
  menu:= TMenuItem.Create( frmMain );
  menu.Add( newItem('􀈕') );
  menu.Add( newItem('􀣰') );
  menu.Add( newItem('􀎞') );
  menu.Add( newItem('􀏅') );
  menu.Add( newItem('􀎶') );
  menu.Add( newItem('􀫀') );
  menu.Add( newItem('􀤋') );
  menu.AddSeparator;
  menu.Add( newItem(' 􀁸') );
  menu.Add( newItem(' 􀀚') );
  menu.Add( newItem(' 􀀄') );
  menu.AddSeparator;
  menu.Add( newItem(' 􀈑') );
  Result:= menu;
end;

function onGetCommandMenu: TMenuItem;
var
  menu: TMenuItem;

  function toItem( source: TMenuItem ): TMenuItem;
  begin
    Result:= TMenuItem.Create( menu );
    Result.Caption:= source.Caption;
    Result.Action:= source.Action;
  end;

  function createShowHotlistMenuItem: TMenuItem;
  begin
    Result:= TMenuItem.Create( menu );
    Result.Caption:= rsMFSTBICommandMenuDirectoryHotlist;
    Result.OnClick:= @toolBarMenuHandler.showHotlist;
  end;

  function createShowFavoriteMenuItem: TMenuItem;
  begin
    Result:= TMenuItem.Create( menu );
    Result.Caption:= rsMFSTBICommandMenuFavoriteTabs;
    Result.OnClick:= @toolBarMenuHandler.showFavoriteTabs;
  end;

  function createQuickLookMenuItem: TMenuItem;
  begin
    Result:= TMenuItem.Create( menu );
    Result.Caption:= rsMFSTBICommandMenuQuickLook;
    Result.OnClick:= @toolBarMenuHandler.showQuickLookPanel;
    Result.ShortCut:= ShortCut( Word(' '), [ssAlt] );
  end;

begin
  menu:= TMenuItem.Create( frmMain );
  menu.Add( toItem(frmMain.miMultiRename) );
  menu.Add( toItem(frmMain.mnuFilesCmpCnt) );
  menu.Add( toItem(frmMain.mnuCmdSyncDirs) );
  menu.AddSeparator;
  menu.Add( toItem(frmMain.mnuCmdSearch) );
  menu.Add( toItem(frmMain.mnuCmdAddNewSearch) );
  menu.Add( toItem(frmMain.mnuCmdViewSearches) );
  menu.AddSeparator;
  menu.Add( createShowHotlistMenuItem );
  menu.Add( toItem(frmMain.mnuCmdConfigDirHotlist) );
  menu.AddSeparator;
  menu.Add( createShowFavoriteMenuItem );
  menu.Add( toItem(frmMain.mnuConfigFavoriteTabs) );
  menu.AddSeparator;
  menu.Add( createQuickLookMenuItem );
  menu.Add( toItem(frmMain.mnuQuickView) );
  menu.Add( toItem(frmMain.mnuFilesShwSysFiles) );
  menu.Add( toItem(frmMain.mnuShowOperations) );
  menu.AddSeparator;
  menu.Add( toItem(frmMain.miEditComment) );
  menu.AddSeparator;
  menu.Add( toItem(frmMain.mnuFilesSymLink) );
  menu.Add( toItem(frmMain.mnuFilesHardLink) );
  menu.AddSeparator;
  menu.Add( toItem(frmMain.mnuSetFileProperties) );
  menu.Add( toItem(frmMain.mnuFilesProperties) );
  menu.AddSeparator;
  menu.Add( toItem(frmMain.mnuCheckSumCalc) );
  menu.Add( toItem(frmMain.mnuCheckSumVerify) );

  Result:= menu;
end;

procedure terminalAction( const Sender: id );
begin
  frmMain.Commands.cm_RunTerm([]);
end;

procedure refreshAction( const Sender: id );
begin
  frmMain.Commands.cm_Refresh([]);
end;

procedure searchFilesAction( const Sender: id );
begin
  frmMain.Commands.cm_Search([]);
end;


const
  treeViewItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.TreeView';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: True;
    iconName: 'sidebar.left';
    title: rsMFSTBITreeViewTitle;
    tips: rsMFSTBITreeViewTips;
    bordered: True;
    onAction: @toggleTreeViewAction;
  );

  horzSplitItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.HorzSplit';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: True;
    iconName: 'rectangle.split.1x2';
    title: rsMFSTBIHorzSplitTitle;
    tips: rsMFSTBIHorzSplitTips;
    bordered: True;
    onAction: @toggleHorzSplitAction;
  );

  swapPanelsItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.SwapPanels';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: True;
    iconName: 'arrow.left.arrow.right.square';
    title: rsMFSTBISwapPanelsTitle;
    tips: rsMFSTBISwapPanelsTips;
    bordered: True;
    onAction: @swapPanelsAction;
  );


  showBriefItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.ShowMode.Brief';
    iconName: 'rectangle.split.3x1';
    title: rsMFSTBIShowBriefTitle;
    tips: '';
    bordered: True;
    onAction: nil;
  );

  showFullItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.ShowMode.Full';
    iconName: 'list.bullet';
    title: rsMFSTBIShowFullTitle;
    tips: '';
    bordered: True;
    onAction: nil;
  );

  showThumbnailsItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.ShowMode.Thumbnails';
    iconName: 'square.grid.2x2';
    title: rsMFSTBIShowThumbnailsTitle;
    tips: '';
    bordered: True;
    onAction: nil;
  );

  showModeItemConfig: TCocoaConfigToolBarItemGroup = (
    identifier: 'MainForm.ShowMode';
    priority: NSToolbarItemVisibilityPriorityHigh;
    iconName: '';
    title: rsMFSTBIShowModeTitle;
    tips: rsMFSTBIShowModeTips;
    bordered: True;
    onAction: @showModeAction;

    representation: NSToolbarItemGroupControlRepresentationAutomatic;
    selectionMode: NSToolbarItemGroupSelectionModeSelectOne;
    selectedIndex: 0;
    subitems: (
    );
  );


  shareItemConfig: TCocoaConfigToolBarItemSharing = (
    identifier: 'MainForm.Share';
    priority: NSToolbarItemVisibilityPriorityUser;
    iconName: '';
    title: rsMFSTBIShareTitle;
    tips: '';
    bordered: True;

    onGetItems: @onGetSharingItems;
  );

  airdropItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.AirDrop';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'airplayaudio';
    title: rsMFSTBIAirDropTitle;
    tips: rsMFSTBIAirDropTips;
    bordered: True;
    onAction: @airdropAction;
  );


  quickLookItemConfig: TCocoaConfigToolBarItem = (
     identifier: 'MainForm.QuickLook';
     priority: NSToolbarItemVisibilityPriorityStandard;
     navigational: False;
     iconName: 'eye';
     title: rsMFSTBIQuickLookTitle;
     tips: rsMFSTBIQuickLookTips;
     bordered: True;
     onAction: @quickLookAction;
   );

  goItemConfig: TCocoaConfigToolBarItemMenu = (
    identifier: 'MainForm.Go';
    iconName: 'folder';
    title: rsMFSTBIGoTitle;
    tips: rsMFSTBIGoTips;
    bordered: True;
    onAction: nil;

    showsIndicator: False;
    menu: nil;
    onGetMenu: @onGetFolderMenu;
  );

  finderRevealItemConfig: TCocoaConfigToolBarItem = (
     identifier: 'MainForm.FinderReveal';
     priority: NSToolbarItemVisibilityPriorityStandard;
     navigational: False;
     iconName: 'faceid';
     title: rsMFSTBIFinderRevealTitle;
     tips: rsMFSTBIFinderRevealTips;
     bordered: True;
     onAction: @finderRevealAction;
   );

  finderInfoItemConfig: TCocoaConfigToolBarItem = (
     identifier: 'MainForm.FinderInfo';
     priority: NSToolbarItemVisibilityPriorityStandard;
     navigational: False;
     iconName: 'info.circle';
     title: rsMFSTBIShowInfoTitle;
     tips: rsMFSTBIShowInfoTips;
     bordered: True;
     onAction: @finderInfoAction;
   );


  commandItemConfig: TCocoaConfigToolBarItemMenu = (
    identifier: 'MainForm.Command';
    iconName: 'ellipsis.circle';
    title: rsMFSTBICommandTitle;
    tips: '';
    bordered: True;
    onAction: nil;

    showsIndicator: True;
    menu: nil;
    onGetMenu: @onGetCommandMenu;
  );


  refreshItemConfig: TCocoaConfigToolBarItem = (
     identifier: 'MainForm.Refresh';
     priority: NSToolbarItemVisibilityPriorityStandard;
     navigational: False;
     iconName: 'arrow.clockwise';
     title: rsMFSTBIRefreshTitle;
     tips: rsMFSTBIRefreshTips;
     bordered: True;
     onAction: @refreshAction;
   );

  terminalItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.Terminal';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'terminal';
    title: rsMFSTBITerminalTitle;
    tips: rsMFSTBITerminalTips;
    bordered: True;
    onAction: @terminalAction;
  );

  searchFilesItemConfig: TCocoaConfigToolBarItem = (
    identifier: 'MainForm.SearchFiles';
    priority: NSToolbarItemVisibilityPriorityStandard;
    navigational: False;
    iconName: 'magnifyingglass';
    title: rsMFSTBISearchTitle;
    tips: rsMFSTBISearchTips;
    bordered: True;
    onAction: @searchFilesAction;
  );

  mainFormConfig: TCocoaConfigForm = (
    name: 'frmMain';
    className: '';
    isMainForm: False;

    titleBar: (
      transparent: False;
      separatorStyle: NSTitlebarSeparatorStyleAutomatic;
    );

    toolBar: (
      identifier: 'MainForm.ToolBar';
      style: NSWindowToolbarStyleAutomatic;
      displayMode: NSToolbarDisplayModeIconOnly;

      allowsUserCustomization: True;
      autosavesConfiguration: False;

      items: (
      );
      defaultItemsIdentifiers: (
        'MainForm.TreeView',
        'MainForm.HorzSplit',
        'MainForm.SwapPanels',

        'MainForm.ShowMode',
        'NSToolbarFlexibleSpaceItem',
        'MainForm.Share',
        'MainForm.AirDrop',
        'NSToolbarFlexibleSpaceItem',
        'MainForm.QuickLook',
        'MainForm.Go',
        'MainForm.FinderReveal',
        'MainForm.FinderInfo',
        'NSToolbarFlexibleSpaceItem',
        'MainForm.Command',
        'NSToolbarFlexibleSpaceItem',
        'MainForm.Terminal',
        'MainForm.Refresh',
        'MainForm.SearchFiles'
      );
      allowedItemsIdentifiers: (
        'MainForm.TreeView',
        'MainForm.HorzSplit',
        'MainForm.SwapPanels',

        'MainForm.ShowMode',
        'MainForm.Share',
        'MainForm.AirDrop',
        'MainForm.QuickLook',
        'MainForm.Go',
        'MainForm.FinderReveal',
        'MainForm.FinderInfo',
        'MainForm.Command',
        'MainForm.Terminal',
        'MainForm.Refresh',
        'MainForm.SearchFiles'
      );
      itemCreator: nil;      // default item Creator
    );
  );

procedure doInitConfig;
begin
  showModeItemConfig.subitems:= [
    TCocoaToolBarUtils.toClass(showBriefItemConfig),
    TCocoaToolBarUtils.toClass(showFullItemConfig),
    TCocoaToolBarUtils.toClass(showThumbnailsItemConfig)
  ];

  mainFormConfig.toolBar.items:= [
    TCocoaToolBarUtils.toClass(treeViewItemConfig),
    TCocoaToolBarUtils.toClass(horzSplitItemConfig),
    TCocoaToolBarUtils.toClass(swapPanelsItemConfig),

    TCocoaToolBarUtils.toClass(showModeItemConfig),
    TCocoaToolBarUtils.toClass(shareItemConfig),
    TCocoaToolBarUtils.toClass(airdropItemConfig),
    TCocoaToolBarUtils.toClass(commandItemConfig),
    TCocoaToolBarUtils.toClass(quickLookItemConfig),
    TCocoaToolBarUtils.toClass(goItemConfig),
    TCocoaToolBarUtils.toClass(finderRevealItemConfig),
    TCocoaToolBarUtils.toClass(finderInfoItemConfig),
    TCocoaToolBarUtils.toClass(terminalItemConfig),
    TCocoaToolBarUtils.toClass(refreshItemConfig),
    TCocoaToolBarUtils.toClass(searchFilesItemConfig)
  ];

  CocoaConfigForms:= [ mainFormConfig ];
end;

procedure initCocoaModernFormConfig;
begin
  if NSAppKitVersionNumber < NSAppKitVersionNumber11_0 then
    Exit;

  fMain.onFileViewUpdated:= @onFileViewUpdated;
  toolBarMenuHandler:= TToolBarMenuHandler.Create;
  doInitConfig;
end;

end.
