(*
    Copyright (c) 2000
        Cambridge University Technical Services Limited

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.
    
    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.
    
    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

structure XCall =
struct
    exception XWindows = RunCall.XWindows;
    
    local
        val callX = RunCall.rtsCallFull1 "PolyXWindowsGeneral"
    in
        fun xcall n = RunCall.unsafeCast(callX(RunCall.unsafeCast n))
    end

(* The following XCALL numbers should match those in xcall_numbers.h *)
(* in the Poly/ML run-time system.                                   *)

  val XCALL_Not                             = 10;
  val XCALL_And                             = 11;
  val XCALL_Or                              = 12;
  val XCALL_Xor                             = 13;
  val XCALL_DownShift                       = 14;
  val XCALL_UpShift                         = 15;
  
  val XCALL_NoDrawable                      = 20;
  val XCALL_NoCursor                        = 21;
  val XCALL_NoFont                          = 22;
  val XCALL_NoColormap                      = 23;
  val XCALL_NoVisual                        = 24;
  
  val XCALL_GetTimeOfDay                    = 30;
  
  (* Colorcells 100 *)
  val XCALL_XAllocColor                     = 103;
  val XCALL_XAllocColorCells                = 104;
  val XCALL_XAllocColorPlanes               = 105;
  val XCALL_XAllocNamedColor                = 106;
  val XCALL_XFreeColors                     = 107;
  val XCALL_XLookupColor                    = 108;
  val XCALL_XParseColor                     = 109;
  val XCALL_XQueryColor                     = 110;
  val XCALL_XQueryColors                    = 111;
  val XCALL_XStoreColor                     = 112;
  val XCALL_XStoreColors                    = 113;
  val XCALL_XStoreNamedColor                = 114;
  val XCALL_BlackPixel                      = 115;
  val XCALL_WhitePixel                      = 116;
  
  (* Colormaps 150 *)
  val XCALL_XCopyColormapAndFree            = 150;
  val XCALL_XCreateColormap                 = 151;
  val XCALL_XInstallColormap                = 154;
  val XCALL_XListInstalledColormaps         = 155;
  val XCALL_XUninstallColormap              = 158;
  val XCALL_DefaultColormap                 = 159;
  val XCALL_DefaultVisual                   = 160;
  val XCALL_DisplayCells                    = 161;
  val XCALL_VisualClass                     = 162;
  val XCALL_VisualRedMask                   = 163;
  val XCALL_VisualGreenMask                 = 164;
  val XCALL_VisualBlueMask                  = 165;
  
  (* Cursors 200 *)
  val XCALL_XCreateFontCursor               = 200;
  val XCALL_XCreateGlyphCursor              = 201;
  val XCALL_XCreatePixmapCursor             = 202;
  val XCALL_XDefineCursor                   = 203;
  val XCALL_XQueryBestCursor                = 205;
  val XCALL_XRecolorCursor                  = 206;
  val XCALL_XUndefineCursor                 = 207;
  
  val XCALL_XOpenDisplay                    = 222;
  
  (* Display Specifications 250 *)
  val XCALL_CellsOfScreen                   = 250;
  val XCALL_DefaultDepth                    = 251;
  val XCALL_DisplayHeight                   = 252;
  val XCALL_DisplayHeightMM                 = 253;
  val XCALL_DisplayPlanes                   = 254;
  val XCALL_DisplayString                   = 255;
  val XCALL_DisplayWidth                    = 256;
  val XCALL_DisplayWidthMM                  = 257;
  val XCALL_DoesBackingStore                = 258;
  val XCALL_DoesSaveUnders                  = 259;
  val XCALL_EventMaskOfScreen               = 260;
  val XCALL_MaxCmapsOfScreen                = 261;
  val XCALL_MinCmapsOfScreen                = 262;
  val XCALL_ProtocolRevision                = 263;
  val XCALL_ProtocolVersion                 = 264;
  val XCALL_ServerVendor                    = 265;
  val XCALL_VendorRelease                   = 266;
  
  (* Drawing Primitives 300 *)
  val XCALL_XClearArea                      = 300;
  val XCALL_XClearWindow                    = 301;
  val XCALL_XCopyArea                       = 302;
  val XCALL_XCopyPlane                      = 303;
  val XCALL_XDrawArc                        = 304;
  val XCALL_XDrawArcs                       = 305;
  val XCALL_XDrawImageString                = 306;
  val XCALL_XDrawImageString16              = 307;
  val XCALL_XDrawLine                       = 308;
  val XCALL_XDrawLines                      = 309;
  val XCALL_XDrawPoint                      = 310;
  val XCALL_XDrawPoints                     = 311;
  val XCALL_XDrawRectangle                  = 312;
  val XCALL_XDrawRectangles                 = 313;
  val XCALL_XDrawSegments                   = 314;
  val XCALL_XDrawString                     = 315;
  val XCALL_XDrawString16                   = 316;
  val XCALL_XDrawText                       = 317;
  val XCALL_XDrawText16                     = 318;
  val XCALL_XFillArc                        = 319;
  val XCALL_XFillArcs                       = 320;
  val XCALL_XFillPolygon                    = 321;
  val XCALL_XFillRectangle                  = 322;
  val XCALL_XFillRectangles                 = 323;
  
  (* Events 350 *)
  val XCALL_XSelectInput                    = 350;
  val XCALL_XSynchronize                    = 351;
  
  val XCALL_GetState                        = 361;
  val XCALL_SetState                        = 362;
  val XCALL_NextEvent                       = 365;
  val XCALL_InsertTimeout                   = 366;
  
  val XCALL_XSetInputFocus                  = 370;
  val XCALL_XGetInputFocus                  = 371;
  val XCALL_XSetSelectionOwner              = 372;
  val XCALL_XGetSelectionOwner              = 373;
  val XCALL_XConvertSelection               = 374;
  val XCALL_XSendSelectionNotify            = 375;
  val XCALL_XDeleteProperty                 = 376;
  val XCALL_XInternAtom                     = 377;
  val XCALL_XGetAtomName                    = 378;
  
  (* Fonts 400 *)
  val XCALL_XGetFontPath                    = 401;
  val XCALL_XListFonts                      = 402;
  val XCALL_XListFontsWithInfo              = 403;
  val XCALL_XLoadFont                       = 404;
  val XCALL_XLoadQueryFont                  = 405;
  val XCALL_XQueryFont                      = 406;
  val XCALL_XSetFontPath                    = 407;
  
  (* Grabbing 450 *)
  
  (* Graphics Context 500 *)
  
  val XCALL_DefaultGC                       = 500;
  val XCALL_UpdateGC                        = 501;
  val XCALL_XCreateGC                       = 503;
  
  val XCALL_XSetClipRectangles              = 509;
  val XCALL_XSetDashes                      = 510;
  
  (* Images 550 *)
  val XCALL_XAddPixel                       = 550;
  val XCALL_XGetImage                       = 552;
  val XCALL_XGetPixel                       = 553;
  val XCALL_XGetSubImage                    = 554;
  val XCALL_XPutImage                       = 555;
  val XCALL_XPutPixel                       = 556;
  val XCALL_XSubImage                       = 557;
  val XCALL_BitmapBitOrder                  = 558;
  val XCALL_BitmapPad                       = 559;
  val XCALL_BitmapUnit                      = 560;
  val XCALL_ByteOrder                       = 561;
  
  (* Keyboard 600 *)
  val XCALL_XLookupString                   = 611;
  val XCALL_XQueryKeymap                    = 612;
  val XCALL_IsCursorKey                     = 618;
  val XCALL_IsFunctionKey                   = 619;
  val XCALL_IsKeypadKey                     = 620;
  val XCALL_IsMiscFunctionKey               = 621;
  val XCALL_IsModifierKey                   = 622;
  val XCALL_IsPFKey                         = 623;
  
  (* Output Buffer 650 *)
  val XCALL_XFlush                          = 650;
  val XCALL_XSync                           = 651;
  
  (* Pointers 700 *)
  val XCALL_XQueryPointer                   = 703;
  
  (* Regions 750 *)
  
  (* Save Set 800 *)
  
  (* Screen Saver 850 *)
  val XCALL_XActivateScreenSaver            = 850;
  val XCALL_XForceScreenSaver               = 851;
  val XCALL_XGetScreenSaver                 = 852;
  val XCALL_XResetScreenSaver               = 853;
  val XCALL_XSetScreenSaver                 = 854;
  
  (* Standard Geometry 900 *)
  val XCALL_XTranslateCoordinates           = 902;
  
  (* Text 950 *)
  val XCALL_XTextExtents                    = 950;
  val XCALL_XTextExtents16                  = 951;
  val XCALL_XTextWidth                      = 952;
  val XCALL_XTextWidth16                    = 953;
  
  (* Tiles, Pixmaps, Stipples and Bitmaps 1000 *)
  val XCALL_XCreateBitmapFromData           = 1000;
  val XCALL_XCreatePixmap                   = 1001;
  val XCALL_XCreatePixmapFromBitmapData     = 1002;
  
  val XCALL_XQueryBestStipple               = 1004;
  val XCALL_XQueryBestTile                  = 1005;
  val XCALL_XReadBitmapFile                 = 1006;
  val XCALL_XWriteBitmapFile                = 1007;
  
  (* User Preferences 1050 *)
  val XCALL_XAutoRepeatOff                  = 1050;
  val XCALL_XAutoRepeatOn                   = 1051;
  val XCALL_XBell                           = 1052;
  val XCALL_XGetDefault                     = 1053;
  
  (* Window Attributes 1100 *)
  val XCALL_ChangeWindow                    = 1100;
  val XCALL_XGetGeometry                    = 1101;
  val XCALL_XGetWindowAttributes            = 1102;
  val XCALL_XSetWindowBorderWidth           = 1107;
  
  (* Window Configuration 1150 *)
  val XCALL_XCirculateSubwindows            = 1150;
  val XCALL_XConfigureWindow                = 1153;
  val XCALL_XLowerWindow                    = 1154;
  val XCALL_XMapRaised                      = 1155;
  val XCALL_XMapSubwindows                  = 1156;
  val XCALL_XMapWindow                      = 1157;
  val XCALL_XMoveResizeWindow               = 1158;
  val XCALL_XMoveWindow                     = 1159;
  val XCALL_XQueryTree                      = 1160;
  val XCALL_XRaiseWindow                    = 1161;
  val XCALL_XReparentWindow                 = 1162;
  val XCALL_XResizeWindow                   = 1163;
  val XCALL_XRestackWindows                 = 1164;
  val XCALL_XUnmapSubwindows                = 1165;
  val XCALL_XUnmapWindow                    = 1166;
  
  (* Window Existence 1200 *)
  val XCALL_RootWindow                      = 1200;
  val XCALL_DestroyXObject                  = 1201;
  val XCALL_XDestroySubwindows              = 1202;
  val XCALL_XCreateSimpleWindow             = 1203;
  val XCALL_XCreateWindow                   = 1204;
  
  (* Window Manager 1250 *)
  val XCALL_XSetProperty                    = 1299;
  val XCALL_XGetTextProperty                = 1250;
  val XCALL_XGetWMHints                     = 1253;
  val XCALL_XGetWMSizeHints                 = 1255;
  val XCALL_XGetIconSizes                   = 1257;
  val XCALL_XGetTransientForHint            = 1259;
  val XCALL_XGetWMColormapWindows           = 1261;
  val XCALL_XGetRGBColormaps                = 1263;
  val XCALL_XWMGeometry                     = 1264;
  
  (* Miscellaneous and Convenience functions 1300 *)
  val XCALL_GetID                           = 1300;
  val XCALL_ResourceExists                  = 1301;
  val XCALL_GetDisplay                      = 1303;
  
  
  (* X Toolkit 3000 *)
  val XCALL_NoWidget                        = 3000;
  
  val XCALL_AppInitialise                   = 3001;
  val XCALL_XtRealizeWidget                 = 3002;
  val XCALL_XtManageChildren                = 3003;
  val XCALL_XtUnmanageChildren              = 3004;
  val XCALL_XtDestroyWidget                 = 3005;
  val XCALL_SetCallbacks                    = 3006;
  val XCALL_XtSetValues                     = 3007;
  val XCALL_GetValue                        = 3008;
  val XCALL_XtParent                        = 3009;
  val XCALL_XtWindow                        = 3010;
  val XCALL_XtDisplay                       = 3011;
  val XCALL_XtUnrealizeWidget               = 3012;
  val XCALL_XtName                          = 3013;
  val XCALL_XtParseTranslationTable         = 3014;
  val XCALL_XtOverrideTranslations          = 3015;
  val XCALL_XtAugmentTranslations           = 3016;
  val XCALL_XtUninstallTranslations         = 3017;
  val XCALL_XtTranslateTablePrint           = 3018;
  val XCALL_XtCreatePopupShell              = 3019;
  val XCALL_InsertWidgetTimeout             = 3020;
  val XCALL_GetWidgetState                  = 3021;
  val XCALL_SetWidgetState                  = 3022;
  val XCALL_XtSetSensitive                  = 3023;
  val XCALL_XtIsSensitive                   = 3024;
  val XCALL_GetSubresources                 = 3025;
  val XCALL_Cast                            = 3026;
  
  (* added 6/12/94 SPF *)
  val XCALL_XtPopup                         = 3027;
  val XCALL_XtPopdown                       = 3028;
  val XCALL_XtMapWidget                     = 3029;
  val XCALL_XtUnmapWidget                   = 3030;
  
  (* added 19/1/95 SPF *)
  val XCALL_XtIsManaged                     = 3031;
  val XCALL_XtIsRealized                    = 3032;

  (* added 23/3/01 DCJM *)
  val XCALL_XtGetApplicationResources       = 3033;
  val XCALL_XtAddEventHandler               = 3034;

  val XCALL_XmCreateArrowButton             = 4000;
  val XCALL_XmCreateArrowButtonGadget       = 4001;
  val XCALL_XmCreateBulletinBoard           = 4002;
  val XCALL_XmCreateBulletinBoardDialog     = 4003;
  val XCALL_XmCreateCascadeButton           = 4004;
  val XCALL_XmCreateCascadeButtonGadget     = 4005;
  val XCALL_XmCreateCommand                 = 4006;
  val XCALL_XmCreateDialogShell             = 4007;
  val XCALL_XmCreateDrawingArea             = 4008;
  val XCALL_XmCreateDrawnButton             = 4009;
  val XCALL_XmCreateErrorDialog             = 4010;
  val XCALL_XmCreateFileSelectionBox        = 4011;
  val XCALL_XmCreateFileSelectionDialog     = 4012;
  val XCALL_XmCreateForm                    = 4013;
  val XCALL_XmCreateFormDialog              = 4014;
  val XCALL_XmCreateFrame                   = 4015;
  val XCALL_XmCreateInformationDialog       = 4016;
  val XCALL_XmCreateLabel                   = 4017;
  val XCALL_XmCreateLabelGadget             = 4018;
  val XCALL_XmCreateList                    = 4019;
  val XCALL_XmCreateMainWindow              = 4020;
  val XCALL_XmCreateMenuBar                 = 4021;
  val XCALL_XmCreateMenuShell               = 4022;
  val XCALL_XmCreateMessageBox              = 4023;
  val XCALL_XmCreateMessageDialog           = 4024;
  val XCALL_XmCreateOptionMenu              = 4025;
  val XCALL_XmCreatePanedWindow             = 4026;
  val XCALL_XmCreatePopupMenu               = 4027;
  val XCALL_XmCreatePromptDialog            = 4028;
  val XCALL_XmCreatePulldownMenu            = 4029;
  val XCALL_XmCreatePushButton              = 4030;
  val XCALL_XmCreatePushButtonGadget        = 4031;
  val XCALL_XmCreateQuestionDialog          = 4032;
  val XCALL_XmCreateRadioBox                = 4033;
  val XCALL_XmCreateRowColumn               = 4034;
  val XCALL_XmCreateScale                   = 4035;
  val XCALL_XmCreateScrollBar               = 4036;
  val XCALL_XmCreateScrolledList            = 4037;
  val XCALL_XmCreateScrolledText            = 4038;
  val XCALL_XmCreateScrolledWindow          = 4039;
  val XCALL_XmCreateSelectionBox            = 4040;
  val XCALL_XmCreateSelectionDialog         = 4041;
  val XCALL_XmCreateSeparator               = 4042;
  val XCALL_XmCreateSeparatorGadget         = 4043;
  val XCALL_XmCreateSimpleCheckBox          = 4044;
  val XCALL_XmCreateSimpleMenuBar           = 4045;
  val XCALL_XmCreateSimpleOptionMenu        = 4046;
  val XCALL_XmCreateSimplePopupMenu         = 4047;
  val XCALL_XmCreateSimplePulldownMenu      = 4048;
  val XCALL_XmCreateSimpleRadioBox          = 4049;
  val XCALL_XmCreateText                    = 4050;
  val XCALL_XmCreateTextField               = 4051;
  val XCALL_XmCreateToggleButton            = 4052;
  val XCALL_XmCreateToggleButtonGadget      = 4053;
  val XCALL_XmCreateWarningDialog           = 4054;
  val XCALL_XmCreateWorkArea                = 4055;
  val XCALL_XmCreateWorkingDialog           = 4056;
  
  val XCALL_XmCascadeButtonHighlight        = 4060;
  val XCALL_XmCommandError                  = 4061;
  val XCALL_XmCommandGetChild               = 4062;
  val XCALL_XmFileSelectionBoxGetChild      = 4063;
  val XCALL_XmFileSelectionDoSearch         = 4064;
  
  val XCALL_XmIsSomething                   = 4080;
  
  val XCALL_XmMainWindowSetAreas            = 4100;
  val XCALL_XmMainWindowSepX                = 4101;
  val XCALL_XmMessageBoxGetChild            = 4105;
  val XCALL_XmOptionButtonGadget            = 4106;
  val XCALL_XmOptionLabelGadget             = 4107;
  val XCALL_XmSelectionBoxGetChild          = 4108;
  
  val XCALL_XmSetMenuCursor                 = 4200;
  val XCALL_XmScrolledWindowSetAreas        = 4201;
  val XCALL_XmTextGetString                 = 4202;
  val XCALL_XmTextSetString                 = 4203;
  val XCALL_XmTextSetInsertionPosition      = 4204;
  
  val XCALL_XmTrackingLocate                = 4210;
  val XCALL_XmUpdateDisplay                 = 4211;


(* XmText widget 4300 *)

(* XmTextClearSelection                       4300; *)
(* XmTextCopy                                 4301; *)
(* XmTextCut                                  4302; *)
  val XCALL_XmTextGetAddMode                = 4303;
  val XCALL_XmTextGetBaseline               = 4304;
  val XCALL_XmTextGetCursorPosition         = 4305;   
  val XCALL_XmTextGetEditable               = 4306;
  val XCALL_XmTextGetInsertionPosition      = 4307;
  val XCALL_XmTextGetLastPosition           = 4308;
  val XCALL_XmTextGetMaxLength              = 4309;
  val XCALL_XmTextGetSelection              = 4310;
(* XmTextGetSelectionPosition                 4311; *)
(* XCALL_XmTextGetString defined above *)
(* XmTextGetSource                            4313; *)
  val XCALL_XmTextGetTopCharacter           = 4314;
  val XCALL_XmTextInsert                    = 4315;
  val XCALL_XmTextPaste                     = 4316;
(* XmTextPosToXY                              4317; *)
  val XCALL_XmTextRemove                    = 4318;
  val XCALL_XmTextReplace                   = 4319;
  val XCALL_XmTextScroll                    = 4320;
  val XCALL_XmTextSetAddMode                = 4321;
  val XCALL_XmTextSetCursorPosition         = 4322;
  val XCALL_XmTextSetEditable               = 4323;
(* XmTextSetHighlight                         4324; *)
(* XCALL_XmTextSetInsertionPosition defined above *)
  val XCALL_XmTextSetMaxLength              = 4326;
(* XmTextSetSelection                         4327; *)
(* XmTextSetSource                            4328; *)
(* XCALL_XmTextSetString defined above *)
  val XCALL_XmTextSetTopCharacter           = 4330;
  val XCALL_XmTextShowPosition              = 4331;
  val XCALL_XmTextXYToPos                   = 4332;


(* XmTextField widget 4350 *)

(* XmTextFieldClearSelection                  4350; *)
(* XmTextFieldCopy                            4351; *)
(* XmTextFieldCut                             4352; *)
  val XCALL_XmTextFieldGetAddMode           = 4353;
  val XCALL_XmTextFieldGetBaseline          = 4354;            
  val XCALL_XmTextFieldGetCursorPosition    = 4355;            
  val XCALL_XmTextFieldGetEditable          = 4356;
  val XCALL_XmTextFieldGetInsertionPosition = 4357;
  val XCALL_XmTextFieldGetLastPosition      = 4358;
  val XCALL_XmTextFieldGetMaxLength         = 4359;
  val XCALL_XmTextFieldGetSelection         = 4360;
(* XmTextFieldGetSelectionPosition            4361; *)
  val XCALL_XmTextFieldGetString            = 4362;
  val XCALL_XmTextFieldInsert               = 4363;
  val XCALL_XmTextFieldPaste                = 4364;
(* XmTextFieldPosToXY                         4365; *)
  val XCALL_XmTextFieldRemove               = 4366;
  val XCALL_XmTextFieldReplace              = 4367;
  val XCALL_XmTextFieldSetAddMode           = 4368;
  val XCALL_XmTextFieldSetCursorPosition    = 4369;
  val XCALL_XmTextFieldSetEditable          = 4370;
(* XmTextFieldSetHighlight                    4371; *)
  val XCALL_XmTextFieldSetInsertionPosition = 4372;
  val XCALL_XmTextFieldSetMaxLength         = 4373;
(* XmTextFieldSetSelection                    4374; *)
  val XCALL_XmTextFieldSetString            = 4375;
  val XCALL_XmTextFieldShowPosition         = 4376;
  val XCALL_XmTextFieldXYToPos              = 4377;


(* XmList widget 4400 *)

  val XCALL_XmListAddItem             = 4400;
  val XCALL_XmListAddItemUnselected   = 4401;
  val XCALL_XmListAddItems            = 4402;
  val XCALL_XmListDeleteAllItems      = 4403;
  val XCALL_XmListDeleteItem          = 4404;
  val XCALL_XmListDeleteItems         = 4405;
  val XCALL_XmListDeletePos           = 4406;
  val XCALL_XmListDeleteItemsPos      = 4407;
  val XCALL_XmListDeselectAllItems    = 4408;
  val XCALL_XmListDeselectItem        = 4409;
  val XCALL_XmListDeselectPos         = 4410;
  val XCALL_XmListGetMatchPos         = 4411;
  val XCALL_XmListGetSelectedPos      = 4412;
  val XCALL_XmListItemExists          = 4413;
  val XCALL_XmListItemPos             = 4414; 
  val XCALL_XmListReplaceItems        = 4415;
  val XCALL_XmListReplaceItemsPos     = 4416;
  val XCALL_XmListSelectItem          = 4417;
  val XCALL_XmListSelectPos           = 4418;
  val XCALL_XmListSetAddMode          = 4419;
  val XCALL_XmListSetBottomItem       = 4420; 
  val XCALL_XmListSetBottomPos        = 4421;
  val XCALL_XmListSetHorizPos         = 4422;
  val XCALL_XmListSetItem             = 4423; 
  val XCALL_XmListSetPos              = 4424;

    (* Added by DCJM. 23/3/01. *)
  val XCALL_XmMenuPosition            = 4500;

(* Not supported by the SUNOS version of the tool-kit ...
  val XCALL_XmListAddItemsUnselected
  val XCALL_XmListDeletePositions
  val XCALL_XmListGetKbdItemPos
  val XCALL_XmListPosSelected
  val XCALL_XmListReplaceItemsPosUnselected
  val XCALL_XmListReplaceItemsPositions 
  val XCALL_XmListSetKbdItemPos 
  val XCALL_XmListUpdateSelectedList 
  val XCALL_XmListYToPos
  val XCALL_XmListPosToBounds 
... *)                        

end;
