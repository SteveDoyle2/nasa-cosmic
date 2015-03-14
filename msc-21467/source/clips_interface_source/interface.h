
#define windowID 		 128
#define AgendaWindowID   129
#define FactsWindowID    130
#define DisplayWindowID  131

#define JoeYesNoAlert   260
#define JoeOkAlert      261
#define CLIPSAlert      262

#define ReadCursorID    256

#define AppleID 128
#define AboutCLIPSItem 1

#define FileID 129
#define NewItem 1
#define OpenItem 2
#define LoadRulesItem 3
#define LoadBatchItem 4
#define OpenDribbleItem 5
#define CloseItem 6
#define SaveItem 8
#define SaveAsItem 9
#define RevertItem 10
#define PageSetupItem 12
#define PrintItem 13
#define TransferItem 15
#define QuitItem 16

#define EditID 130
#define UndoItem 1
#define CutItem 3
#define CopyItem 4
#define PasteItem 5
#define ClearItem 6
#define BalanceItem 8
#define SetFontItem 10
#define OptionsItem 12

#define BufferID             131
#define FindItem               1
#define FindAgainItem          2
#define ReplaceItem            3
#define ReplaceAndFindItem     4
#define ReplaceAllItem         5
#define CompileSelectionItem   7
#define CompileBufferItem      8


/* Execution menu command indices */
#define ExecutionID 132
#define ResetItem     1
#define RunItem       2
#define StepItem      3
#define CLIPSClearItem     5
#define NoMoJoItem    7

/* Watch menu command indices */
#define WatchID     133
#define FactsItem       1
#define RulesItem       2
#define ActivationsItem 3
#define CompilationsItem 4
#define AllItem		  6
#define NoneItem        7

/* Browse menu command indices */
#define BrowseID     134
#define ShowFactsItem            1
#define ShowRulesItem            2
#define ShowAgendaItem           3
#define ShowDeffactsItem         4
#define ShowDeftemplatesItem     5
#define RuleManagerItem          7
#define DeffactsManagerItem      8
#define DeftemplateManagerItem   9
#define AgendaManagerItem       10
#define FactsWindowItem         12
#define AgendaWindowItem        13

#define aaSave		1
#define aaDiscard	2
#define aaCancel	3


#ifndef NULL
#define NULL 0L
#endif


#define MenuBarHeight 20
#define TitleBarHeight 18
#define ScreenMargin 4

#define MinWidth 80
#define MinHeight 80
#define SBarWidth 16
#define TextMargin 4

#define DlgTop 100
#define DlgLeft 85

#define SetFontID 266
#define AboutID 1000
#define SaveID 1001
#define RevertID 1002
#define CantPrintID 1003
#define WrongTypeID 1004
#define OpWrID 1005
#define IOErrID 1006
#define StopCantDoID 1009

#define NoTitleID 1000


#define hOffset 15
#define vOffset 20

#define saveItem 1
#define discardItem 2
#define cancelItem 3

#define activeValue 0
#define inactiveValue 255

#define ToTop 0
#define ToBottom 1
#define ToMiddle 2

#define MAX_BUFFER_SIZE (32767L)

#define VIRTUAL_HORIZONTAL_WIDTH 2000
#define HORIZONTAL_SCROLL_INCREMENT 10

/***********************************/
/* Definitions for undo operation. */
/***********************************/

#define NO_EDIT  0
#define UNPASTE  1
#define REPASTE  2
#define UNCUT    3
#define RECUT    4
#define UNCLEAR  5
#define RECLEAR  6
#define UNTYPING 7
#define RETYPING 8


