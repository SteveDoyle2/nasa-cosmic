
typedef	struct WindowData
	{
	 TEHandle editRec;
	 ControlHandle vScrollBar;
	 ControlHandle hScrollBar;
	 int dirty;
	 Byte padding;
	 int volNumber;
	 int fileNumber;
	 int placementPos;
	} WindowData, *WDPtr, **WDHandle;
