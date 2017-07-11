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

(* Note that this module depends on the compiler's *representation* of
   exceptions; this is absolutely disgusting, completely unnecessary, and
   a source of inefficiency (all those Casts!). When I have some spare time,
   I'm going to cure this. SPF 6/12/94 *)
structure MotifCore =
struct

  local
    open XWindows;
    open XCall;
    open MotifTypes;
    open XmTypes;
    
    fun fromAlignment XmALIGNMENT_BEGINNING = 0
    |   fromAlignment XmALIGNMENT_CENTER    = 1
    |   fromAlignment XmALIGNMENT_END       = 2;
    
    fun toAlignment 0 = XmALIGNMENT_BEGINNING
    |   toAlignment 1 = XmALIGNMENT_CENTER
    |   toAlignment _ = XmALIGNMENT_END;
    
    fun fromArrowDirection XmARROW_UP    = 0
    |   fromArrowDirection XmARROW_DOWN  = 1
    |   fromArrowDirection XmARROW_LEFT  = 2
    |   fromArrowDirection XmARROW_RIGHT = 3;
    
    fun toArrowDirection 0 = XmARROW_UP
    |   toArrowDirection 1 = XmARROW_DOWN
    |   toArrowDirection 2 = XmARROW_LEFT
    |   toArrowDirection _ = XmARROW_RIGHT;

    fun fromAttachment XmATTACH_NONE            = 0
    |   fromAttachment XmATTACH_FORM            = 1
    |   fromAttachment XmATTACH_OPPOSITE_FORM   = 2
    |   fromAttachment XmATTACH_WIDGET          = 3
    |   fromAttachment XmATTACH_OPPOSITE_WIDGET = 4
    |   fromAttachment XmATTACH_POSITION        = 5
    |   fromAttachment XmATTACH_SELF            = 6;
    
    fun toAttachment 0 = XmATTACH_NONE
    |   toAttachment 1 = XmATTACH_FORM
    |   toAttachment 2 = XmATTACH_OPPOSITE_FORM
    |   toAttachment 3 = XmATTACH_WIDGET
    |   toAttachment 4 = XmATTACH_OPPOSITE_WIDGET
    |   toAttachment 5 = XmATTACH_POSITION
    |   toAttachment _ = XmATTACH_SELF;
    
    fun fromButtonType XmPUSHBUTTON       = 1
    |   fromButtonType XmCHECKBUTTON      = 2
    |   fromButtonType XmTOGGLEBUTTON     = 2   (* same as XmCHECKBUTTON *)
    |   fromButtonType XmRADIOBUTTON      = 3
    |   fromButtonType XmCASCADEBUTTON    = 4
    |   fromButtonType XmSEPARATOR        = 5
    |   fromButtonType XmDOUBLE_SEPARATOR = 6
    |   fromButtonType XmTITLE            = 7;
    
    fun toButtonType 1 = XmPUSHBUTTON
    |   toButtonType 2 = XmCHECKBUTTON
    |   toButtonType 3 = XmRADIOBUTTON
    |   toButtonType 4 = XmCASCADEBUTTON
    |   toButtonType 5 = XmSEPARATOR
    |   toButtonType 6 = XmDOUBLE_SEPARATOR
    |   toButtonType _ = XmTITLE;
    
    fun fromCommandWindowLocation XmCOMMAND_ABOVE_WORKSPACE = 0
    |   fromCommandWindowLocation XmCOMMAND_BELOW_WORKSPACE = 1;

    fun toCommandWindowLocation 0 = XmCOMMAND_ABOVE_WORKSPACE
    |   toCommandWindowLocation _ = XmCOMMAND_BELOW_WORKSPACE;

    fun fromDefaultButtonType XmDIALOG_NONE             = 0
    |   fromDefaultButtonType XmDIALOG_APPLY_BUTTON     = 1
    |   fromDefaultButtonType XmDIALOG_CANCEL_BUTTON    = 2
    |   fromDefaultButtonType XmDIALOG_DEFAULT_BUTTON   = 3
    |   fromDefaultButtonType XmDIALOG_OK_BUTTON        = 4
    |   fromDefaultButtonType XmDIALOG_FILTER_LABEL     = 5
    |   fromDefaultButtonType XmDIALOG_FILTER_TEXT      = 6
    |   fromDefaultButtonType XmDIALOG_HELP_BUTTON      = 7
    |   fromDefaultButtonType XmDIALOG_LIST             = 8
    |   fromDefaultButtonType XmDIALOG_LIST_LABEL       = 9
    |   fromDefaultButtonType XmDIALOG_MESSAGE_LABEL    = 10
    |   fromDefaultButtonType XmDIALOG_SELECTION_LABEL  = 11
    |   fromDefaultButtonType XmDIALOG_SYMBOL_LABEL     = 12
    |   fromDefaultButtonType XmDIALOG_TEXT             = 13
    |   fromDefaultButtonType XmDIALOG_SEPARATOR        = 14
    |   fromDefaultButtonType XmDIALOG_DIR_LIST         = 15
    |   fromDefaultButtonType XmDIALOG_DIR_LIST_LABEL   = 16;

    fun toDefaultButtonType 0  = XmDIALOG_NONE
    |   toDefaultButtonType 1  = XmDIALOG_APPLY_BUTTON
    |   toDefaultButtonType 2  = XmDIALOG_CANCEL_BUTTON
    |   toDefaultButtonType 3  = XmDIALOG_DEFAULT_BUTTON
    |   toDefaultButtonType 4  = XmDIALOG_OK_BUTTON
    |   toDefaultButtonType 5  = XmDIALOG_FILTER_LABEL
    |   toDefaultButtonType 6  = XmDIALOG_FILTER_TEXT
    |   toDefaultButtonType 7  = XmDIALOG_HELP_BUTTON
    |   toDefaultButtonType 8  = XmDIALOG_LIST
    |   toDefaultButtonType 9  = XmDIALOG_LIST_LABEL
    |   toDefaultButtonType 10 = XmDIALOG_MESSAGE_LABEL
    |   toDefaultButtonType 11 = XmDIALOG_SELECTION_LABEL
    |   toDefaultButtonType 12 = XmDIALOG_SYMBOL_LABEL
    |   toDefaultButtonType 13 = XmDIALOG_TEXT
    |   toDefaultButtonType 14 = XmDIALOG_SEPARATOR
    |   toDefaultButtonType 15 = XmDIALOG_DIR_LIST
    |   toDefaultButtonType _  = XmDIALOG_DIR_LIST_LABEL;
    
    fun fromDeleteResponse XmDESTROY    = 0
    |   fromDeleteResponse XmUNMAP      = 1
    |   fromDeleteResponse XmDO_NOTHING = 2;
    
    fun toDeleteResponse 0 = XmDESTROY
    |   toDeleteResponse 1 = XmUNMAP
    |   toDeleteResponse _ = XmDO_NOTHING;
    
    fun fromDialogStyle XmDIALOG_MODELESS                  = 0
    |   fromDialogStyle XmDIALOG_PRIMARY_APPLICATION_MODAL = 1
    |   fromDialogStyle XmDIALOG_FULL_APPLICATION_MODAL    = 2
    |   fromDialogStyle XmDIALOG_SYSTEM_MODAL              = 3;

    fun toDialogStyle 0 = XmDIALOG_MODELESS
    |   toDialogStyle 1 = XmDIALOG_PRIMARY_APPLICATION_MODAL
    |   toDialogStyle 2 = XmDIALOG_FULL_APPLICATION_MODAL
    |   toDialogStyle _ = XmDIALOG_SYSTEM_MODAL;
    
    fun fromDialogType XmDIALOG_ERROR          = 1   (* XmMessageBox values *)
    |   fromDialogType XmDIALOG_INFORMATION    = 2
    |   fromDialogType XmDIALOG_MESSAGE        = 3
    |   fromDialogType XmDIALOG_QUESTION       = 4
    |   fromDialogType XmDIALOG_WARNING        = 5
    |   fromDialogType XmDIALOG_WORKING        = 6
    |   fromDialogType XmDIALOG_WORK_AREA      = 0   (* XmSelectionBox values *)
    |   fromDialogType XmDIALOG_PROMPT         = 1
    |   fromDialogType XmDIALOG_SELECTION      = 2
    |   fromDialogType XmDIALOG_COMMAND        = 3
    |   fromDialogType XmDIALOG_FILE_SELECTION = 4;

    fun toDialogType 0 = XmDIALOG_WORK_AREA
    |   toDialogType 1 = XmDIALOG_ERROR
    |   toDialogType 2 = XmDIALOG_INFORMATION
    |   toDialogType 3 = XmDIALOG_MESSAGE
    |   toDialogType 4 = XmDIALOG_QUESTION
    |   toDialogType 5 = XmDIALOG_WARNING
    |   toDialogType _ = XmDIALOG_WORKING;

    fun fromEditMode XmMULTI_LINE_EDIT  = 0
    |   fromEditMode XmSINGLE_LINE_EDIT = 1;
    
    fun toEditMode 0 = XmMULTI_LINE_EDIT
    |   toEditMode _ = XmSINGLE_LINE_EDIT;
    
    fun fromFileTypeMask XmFILE_REGULAR   = 1
    |   fromFileTypeMask XmFILE_DIRECTORY = 2
    |   fromFileTypeMask XmFILE_ANY_TYPE  = 3;
    
    fun toFileTypeMask 1 = XmFILE_REGULAR
    |   toFileTypeMask 2 = XmFILE_DIRECTORY
    |   toFileTypeMask _ = XmFILE_ANY_TYPE;
    
    fun fromIndicatorType XmN_OF_MANY   = 1
    |   fromIndicatorType XmONE_OF_MANY = 2;
    
    fun toIndicatorType 1 = XmN_OF_MANY
    |   toIndicatorType _ = XmONE_OF_MANY;
    
    fun fromKeyboardFocusPolicy XmEXPLICIT = 0
    |   fromKeyboardFocusPolicy XmPOINTER  = 1;
    
    fun toKeyboardFocusPolicy 0 = XmEXPLICIT
    |   toKeyboardFocusPolicy _ = XmPOINTER;
    
    fun fromLabelType XmPIXMAP = 1
    |   fromLabelType XmSTRING = 2;
    
    fun toLabelType 1 = XmPIXMAP
    |   toLabelType _ = XmSTRING;
    
    fun fromMultiClick XmMULTICLICK_DISCARD = 0
    |   fromMultiClick XmMULTICLICK_KEEP    = 1;
    
    fun toMultiClick 0 = XmMULTICLICK_DISCARD
    |   toMultiClick _ = XmMULTICLICK_KEEP;
    
    fun fromNavigationType XmNONE                = 0
    |   fromNavigationType XmTAB_GROUP           = 1
    |   fromNavigationType XmSTICKY_TAB_GROUP    = 2
    |   fromNavigationType XmEXCLUSIVE_TAB_GROUP = 3;

    fun toNavigationType 0 = XmNONE
    |   toNavigationType 1 = XmTAB_GROUP
    |   toNavigationType 2 = XmSTICKY_TAB_GROUP
    |   toNavigationType _ = XmEXCLUSIVE_TAB_GROUP;

    fun fromOrientation XmVERTICAL   = 1
    |   fromOrientation XmHORIZONTAL = 2;
    
    fun toOrientation 1 = XmVERTICAL
    |   toOrientation _ = XmHORIZONTAL;
      
    fun fromResizePolicy XmRESIZE_NONE = 0
    |   fromResizePolicy XmRESIZE_GROW = 1
    |   fromResizePolicy XmRESIZE_ANY  = 2;
    
    fun toResizePolicy 0 = XmRESIZE_NONE
    |   toResizePolicy 1 = XmRESIZE_GROW
    |   toResizePolicy _ = XmRESIZE_ANY;
    
    fun fromRowColumnType XmWORK_AREA     = 0
    |   fromRowColumnType XmMENU_BAR      = 1
    |   fromRowColumnType XmMENU_PULLDOWN = 2
    |   fromRowColumnType XmMENU_POPUP    = 3
    |   fromRowColumnType XmMENU_OPTION   = 4;
    
    fun toRowColumnType 0 = XmWORK_AREA
    |   toRowColumnType 1 = XmMENU_BAR
    |   toRowColumnType 2 = XmMENU_PULLDOWN
    |   toRowColumnType 3 = XmMENU_POPUP
    |   toRowColumnType _ = XmMENU_OPTION;
    
    fun fromPacking XmPACK_TIGHT  = 1
    |   fromPacking XmPACK_COLUMN = 2
    |   fromPacking XmPACK_NONE   = 3;
    
    fun toPacking 1 = XmPACK_TIGHT
    |   toPacking 2 = XmPACK_COLUMN
    |   toPacking _ = XmPACK_NONE;
    
    fun fromProcessingDirection XmMAX_ON_TOP    = 0
    |   fromProcessingDirection XmMAX_ON_BOTTOM = 1
    |   fromProcessingDirection XmMAX_ON_LEFT   = 2
    |   fromProcessingDirection XmMAX_ON_RIGHT  = 3;
    
    fun toProcessingDirection 0 = XmMAX_ON_TOP
    |   toProcessingDirection 1 = XmMAX_ON_BOTTOM
    |   toProcessingDirection 2 = XmMAX_ON_LEFT
    |   toProcessingDirection _ = XmMAX_ON_RIGHT;
    
    fun fromScrollBarDisplayPolicy XmSTATIC    = 0
    |   fromScrollBarDisplayPolicy XmAS_NEEDED = 1;
    
    fun toScrollBarDisplayPolicy 0 = XmSTATIC
    |   toScrollBarDisplayPolicy _ = XmAS_NEEDED;
    
    fun fromScrollBarPlacement XmBOTTOM_RIGHT = 0
    |   fromScrollBarPlacement XmTOP_RIGHT    = 1
    |   fromScrollBarPlacement XmBOTTOM_LEFT  = 2
    |   fromScrollBarPlacement XmTOP_LEFT     = 3;
    
    fun toScrollBarPlacement 0 = XmBOTTOM_RIGHT
    |   toScrollBarPlacement 1 = XmTOP_RIGHT
    |   toScrollBarPlacement 2 = XmBOTTOM_LEFT
    |   toScrollBarPlacement _ = XmTOP_LEFT;
    
    fun fromScrollingPolicy XmAUTOMATIC           = 0
    |   fromScrollingPolicy XmAPPLICATION_DEFINED = 1;
    
    fun toScrollingPolicy 0 = XmAUTOMATIC
    |   toScrollingPolicy _ = XmAPPLICATION_DEFINED;
    
    fun fromSelectionPolicy XmSINGLE_SELECT   = 0
    |   fromSelectionPolicy XmMULTIPLE_SELECT = 1
    |   fromSelectionPolicy XmEXTENDED_SELECT = 2
    |   fromSelectionPolicy XmBROWSE_SELECT   = 3;
    
    fun toSelectionPolicy 0 = XmSINGLE_SELECT
    |   toSelectionPolicy 1 = XmMULTIPLE_SELECT
    |   toSelectionPolicy 2 = XmEXTENDED_SELECT
    |   toSelectionPolicy _ = XmBROWSE_SELECT;
    
    fun fromShadowType XmNO_LINE            = 0
    |   fromShadowType XmSINGLE_LINE        = 1
    |   fromShadowType XmDOUBLE_LINE        = 2
    |   fromShadowType XmSINGLE_DASHED_LINE = 3
    |   fromShadowType XmDOUBLE_DASHED_LINE = 4
    |   fromShadowType XmSHADOW_ETCHED_IN   = 5
    |   fromShadowType XmSHADOW_ETCHED_OUT  = 6
    |   fromShadowType XmSHADOW_IN          = 7
    |   fromShadowType XmSHADOW_OUT         = 8;
    
    fun toShadowType 0 = XmNO_LINE
    |   toShadowType 1 = XmSINGLE_LINE
    |   toShadowType 2 = XmDOUBLE_LINE
    |   toShadowType 3 = XmSINGLE_DASHED_LINE
    |   toShadowType 4 = XmDOUBLE_DASHED_LINE
    |   toShadowType 5 = XmSHADOW_ETCHED_IN
    |   toShadowType 6 = XmSHADOW_ETCHED_OUT
    |   toShadowType 7 = XmSHADOW_IN
    |   toShadowType _ = XmSHADOW_OUT;
    
    fun fromStringDirection XmSTRING_DIRECTION_L_TO_R  = 0
    |   fromStringDirection XmSTRING_DIRECTION_R_TO_L  = 1
    |   fromStringDirection XmSTRING_DIRECTION_DEFAULT = 255;

    fun toStringDirection 0 = XmSTRING_DIRECTION_L_TO_R
    |   toStringDirection 1 = XmSTRING_DIRECTION_R_TO_L
    |   toStringDirection _ = XmSTRING_DIRECTION_DEFAULT;

    fun fromTextScanType XmSELECT_POSITION   = 0
    |   fromTextScanType XmSELECT_WHITESPACE = 1
    |   fromTextScanType XmSELECT_WORD       = 2
    |   fromTextScanType XmSELECT_LINE       = 3
    |   fromTextScanType XmSELECT_ALL        = 4
    |   fromTextScanType XmSELECT_PARAGRAPH  = 5;

    fun toTextScanType 0 = XmSELECT_POSITION
    |   toTextScanType 1 = XmSELECT_WHITESPACE
    |   toTextScanType 2 = XmSELECT_WORD
    |   toTextScanType 3 = XmSELECT_LINE
    |   toTextScanType 4 = XmSELECT_ALL
    |   toTextScanType _ = XmSELECT_PARAGRAPH;

    fun fromUnitType XmPIXELS            = 0
    |   fromUnitType Xm100TH_MILLIMETERS = 1
    |   fromUnitType Xm1000TH_INCHES     = 2
    |   fromUnitType Xm100TH_POINTS      = 3
    |   fromUnitType Xm100TH_FONT_UNITS  = 4;

    fun toUnitType 0 = XmPIXELS
    |   toUnitType 1 = Xm100TH_MILLIMETERS
    |   toUnitType 2 = Xm1000TH_INCHES
    |   toUnitType 3 = Xm100TH_POINTS
    |   toUnitType _ = Xm100TH_FONT_UNITS;

    fun fromVisualPolicy XmVARIABLE           = 0
    |   fromVisualPolicy XmCONSTANT           = 1
    |   fromVisualPolicy XmRESIZE_IF_POSSIBLE = 2;
    
    fun toVisualPolicy 0 = XmVARIABLE
    |   toVisualPolicy 1 = XmCONSTANT
    |   toVisualPolicy _ = XmRESIZE_IF_POSSIBLE;
    
    fun fromXWMStateHint DontCareState = 0
    |   fromXWMStateHint NormalState   = 1
    |   fromXWMStateHint ZoomState     = 2
    |   fromXWMStateHint IconicState   = 3
    |   fromXWMStateHint InactiveState = 4;
    
    fun toXWMStateHint 0 = DontCareState
    |   toXWMStateHint 1 = NormalState
    |   toXWMStateHint 2 = ZoomState
    |   toXWMStateHint 3 = IconicState
    |   toXWMStateHint _ = InactiveState;

    (* When this datatype changes the runtime system must change as well. *)
    (* I've removed this and replaced it simply by a tuple but making the tuple
       represent the values in the same way.  That makes the interface independent
       of the way datatypes are represented. Note: the old representation of
       datatypes put the tag in the second word. DCJM 27/3/01. *)
    (*
    datatype CType = 
      CAccelerators  of XtAccelerators
    | CBool          of bool                       (* typedef char Boolean *)
    | CColormap      of Colormap
    | CCursor        of Cursor
    | CDimension     of int                        (* typedef unsigned short Dimension *)
    | CFontList      of (XFontStruct * string) list
    | CInt           of int
    | CIntTable      of int list
    | CKeySym        of int                        (* typedef unsigned int KeySym *)
    | CPixmap        of Drawable
    | CPosition      of int                        (* typedef short Position *)
    | CString        of string
    | CStringTable   of string list
    | CTrans         of XtTranslations
    | CUnsignedChar  of int
    | CUnsignedTable of int list                   (* typedef uchar * *)
    | CVisual        of Visual
    | CWidget        of Widget
    | CWidgetList    of Widget list
    | CXmString      of string
    | CXmStringTable of string list;
    *)
    val cAccelerators  = 1
    and cBool          = 2
    and cColormap      = 3
    and cCursor        = 4
    and cDimension     = 5
    and cFontList      = 6
    and cInt           = 7
    and cIntTable      = 8
    and cKeySym        = 9
    and cPixmap        = 10
    and cPosition      = 11
    and cString        = 12
    and cStringTable   = 13
    and cTrans         = 14
    and cUnsignedChar  = 15
    and cUnsignedTable = 16
    and cVisual        = 17
    and cWidget        = 18
    and cWidgetList    = 19
    and cXmString      = 20
    and cXmStringTable = 21
    
    fun I x = x;
    
    (* This version can be inlined for greater efficiency. SPF 1/6/95 *)
    val Cast : 'a -> 'b = RunCall.unsafeCast;

    val toInt : 'a -> int = Cast (* we could make this any type. *)

    fun CType (XmRAcceleratorTable       a) = (toInt a,                       cAccelerators)
    |   CType (XmRAlignment              a) = (fromAlignment a,               cUnsignedChar)
    |   CType (XmRArrowDirection         a) = (fromArrowDirection a,          cUnsignedChar)
    |   CType (XmRAtom                   i) = (i,                             cKeySym)  (* Atom = unsigned = KeySym *)
    |   CType (XmRAttachment             a) = (fromAttachment     a,          cUnsignedChar)
    |   CType (XmRBool                   b) = (toInt b,                       cBool)
    |   CType (XmRButtonTypeTable        L) = (toInt(map fromButtonType L),   cUnsignedTable)
    |   CType (XmRCardinal               i) = (i,                             cKeySym)  (* Cardinal = unsigned = KeySym *)
    |   CType (XmRColormap               c) = (toInt c,                       cColormap)
    |   CType (XmRCommandWindowLocation  t) = (fromCommandWindowLocation t,   cUnsignedChar)
    |   CType (XmRCursor                 c) = (toInt c,                       cCursor)
    |   CType (XmRDefaultButtonType      e) = (fromDefaultButtonType e,       cInt)
    |   CType (XmRDeleteResponse         e) = (fromDeleteResponse    e,       cInt)
    |   CType (XmRDialogStyle            e) = (fromDialogStyle       e,       cInt)
    |   CType (XmRDialogType             e) = (fromDialogType        e,       cUnsignedChar)
    |   CType (XmRDimension              d) = (d,                             cDimension)
    |   CType (XmREditMode               e) = (fromEditMode          e,       cInt)
    |   CType (XmRFileTypeMask           m) = (fromFileTypeMask      m,       cUnsignedChar)
    |   CType (XmRFontList               L) = (toInt L,                       cFontList)
    |   CType (XmRIndicatorType          t) = (fromIndicatorType t,           cUnsignedChar)
    |   CType (XmRInitialState           i) = (fromXWMStateHint  i,           cInt)
    |   CType (XmRInt                    i) = (i,                             cInt)
    |   CType (XmRKeySym                 k) = (k,                             cKeySym)
    |   CType (XmRKeySymTable            k) = (toInt k,                       cIntTable)
    |   CType (XmRKeyboardFocusPolicy    t) = (fromKeyboardFocusPolicy t,     cUnsignedChar)
    |   CType (XmRLabelType              t) = (fromLabelType           t,     cUnsignedChar)
    |   CType (XmRMultiClick             t) = (fromMultiClick          t,     cUnsignedChar)
    |   CType (XmRNavigationType         t) = (fromNavigationType      t,     cUnsignedChar)
    |   CType (XmROrientation            r) = (fromOrientation         r,     cUnsignedChar)
    |   CType (XmRPacking                p) = (fromPacking             p,     cUnsignedChar)
    |   CType (XmRProcessingDirection    p) = (fromProcessingDirection p,     cUnsignedChar)
    |   CType (XmRResizePolicy           p) = (fromResizePolicy        p,     cUnsignedChar)
    |   CType (XmRRowColumnType          p) = (fromRowColumnType       p,     cUnsignedChar)
    |   CType (XmRPixel                  p) = (p,                             cInt)
    |   CType (XmRPixmap                 p) = (toInt p,                       cPixmap)
    |   CType (XmRPosition               p) = (p,                             cPosition)
    |   CType (XmRScrollBarDisplayPolicy p) = (fromScrollBarDisplayPolicy p,  cUnsignedChar)
    |   CType (XmRScrollBarPlacement     p) = (fromScrollBarPlacement     p,  cUnsignedChar)
    |   CType (XmRScrollingPolicy        p) = (fromScrollingPolicy        p,  cUnsignedChar)
    |   CType (XmRSelectionPolicy        p) = (fromSelectionPolicy        p,  cUnsignedChar)
    |   CType (XmRShadowType             t) = (fromShadowType             t,  cUnsignedChar)
    |   CType (XmRShort                  i) = (i,                             cPosition) (* Position = short *)
    |   CType (XmRStringDirection        t) = (fromStringDirection        t,  cUnsignedChar)
    |   CType (XmRString                 s) = (toInt s,                       cString)
    |   CType (XmRStringTable            s) = (toInt s,                       cStringTable)
    |   CType (XmRTextScanTypeTable      L) = (toInt(map fromTextScanType L), cIntTable)
    |   CType (XmRTranslationTable       t) = (toInt t,                       cTrans)
    |   CType (XmRUnitType               t) = (fromUnitType t,                cUnsignedChar)
    |   CType (XmRVisual                 v) = (toInt v,                       cVisual)
    |   CType (XmRVisualPolicy           p) = (fromVisualPolicy p,            cUnsignedChar)
    |   CType (XmRWidget                 w) = (toInt w,                       cWidget)
    |   CType (XmRWidgetList             L) = (toInt L,                       cWidgetList)
    |   CType (XmRXmString               s) = (toInt s,                       cXmString)
    |   CType (XmRXmStringTable          t) = (toInt t,                       cXmStringTable);

    fun Convert (XmRAcceleratorTable       _) = I
      | Convert (XmRAlignment              _) = Cast toAlignment
      | Convert (XmRArrowDirection         _) = Cast toArrowDirection
      | Convert (XmRAtom                   _) = I
      | Convert (XmRAttachment             _) = Cast toAttachment
      | Convert (XmRBool                   _) = I
      | Convert (XmRButtonTypeTable        _) = Cast (map toButtonType)
      | Convert (XmRCardinal               _) = I
      | Convert (XmRColormap               _) = I
      | Convert (XmRCommandWindowLocation  _) = Cast toCommandWindowLocation
      | Convert (XmRCursor                 _) = I
      | Convert (XmRDefaultButtonType      _) = Cast toDefaultButtonType
      | Convert (XmRDeleteResponse         _) = Cast toDeleteResponse
      | Convert (XmRDialogStyle            _) = Cast toDialogStyle
      | Convert (XmRDialogType             _) = Cast toDialogType
      | Convert (XmRDimension              _) = I
      | Convert (XmREditMode               _) = Cast toEditMode
      | Convert (XmRFileTypeMask           _) = Cast toFileTypeMask
      | Convert (XmRFontList               _) = I
      | Convert (XmRIndicatorType          _) = Cast toIndicatorType
      | Convert (XmRInitialState           _) = Cast toXWMStateHint
      | Convert (XmRInt                    _) = I
      | Convert (XmRKeySym                 _) = I
      | Convert (XmRKeySymTable            _) = I
      | Convert (XmRKeyboardFocusPolicy    _) = Cast toKeyboardFocusPolicy
      | Convert (XmRLabelType              _) = Cast toLabelType
      | Convert (XmRMultiClick             _) = Cast toMultiClick
      | Convert (XmRNavigationType         _) = Cast toNavigationType
      | Convert (XmROrientation            _) = Cast toOrientation
      | Convert (XmRPacking                _) = Cast toPacking
      | Convert (XmRProcessingDirection    _) = Cast toProcessingDirection
      | Convert (XmRResizePolicy           _) = Cast toResizePolicy
      | Convert (XmRRowColumnType          _) = Cast toRowColumnType
      | Convert (XmRPixel                  _) = I
      | Convert (XmRPixmap                 _) = I
      | Convert (XmRPosition               _) = I
      | Convert (XmRScrollBarDisplayPolicy _) = Cast toScrollBarDisplayPolicy
      | Convert (XmRScrollBarPlacement     _) = Cast toScrollBarPlacement
      | Convert (XmRScrollingPolicy        _) = Cast toScrollingPolicy
      | Convert (XmRSelectionPolicy        _) = Cast toSelectionPolicy
      | Convert (XmRShadowType             _) = Cast toShadowType
      | Convert (XmRShort                  _) = I
      | Convert (XmRStringDirection        _) = Cast toStringDirection
      | Convert (XmRString                 _) = I
      | Convert (XmRStringTable            _) = I
      | Convert (XmRTextScanTypeTable      _) = Cast (map toTextScanType)
      | Convert (XmRTranslationTable       _) = I
      | Convert (XmRUnitType               _) = Cast toUnitType
      | Convert (XmRVisual                 _) = I
      | Convert (XmRVisualPolicy           _) = Cast toVisualPolicy
      | Convert (XmRWidget                 _) = I
      | Convert (XmRWidgetList             _) = I
      | Convert (XmRXmString               _) = I
      | Convert (XmRXmStringTable          _) = I;
    
    fun RName (XmRAcceleratorTable       _) = "AcceleratorTable"
    |   RName (XmRAlignment              _) = "Alignment"
    |   RName (XmRArrowDirection         _) = "ArrowDirection"
    |   RName (XmRAtom                   _) = "Atom"
    |   RName (XmRAttachment             _) = "Attachment"
    |   RName (XmRBool                   _) = "Bool"
    |   RName (XmRButtonTypeTable        _) = "ButtonTypeTable"
    |   RName (XmRCardinal               _) = "Cardinal"
    |   RName (XmRColormap               _) = "Colormap"
    |   RName (XmRCommandWindowLocation  _) = "CommandWindowLocation"
    |   RName (XmRCursor                 _) = "Cursor"
    |   RName (XmRDefaultButtonType      _) = "DefaultButtonType"
    |   RName (XmRDeleteResponse         _) = "DeleteResponse"
    |   RName (XmRDialogStyle            _) = "DialogStyle"
    |   RName (XmRDialogType             _) = "DialogType"
    |   RName (XmRDimension              _) = "Dimension"
    |   RName (XmREditMode               _) = "EditMode"
    |   RName (XmRFileTypeMask           _) = "FileTypeMask"
    |   RName (XmRFontList               _) = "FontList"
    |   RName (XmRIndicatorType          _) = "IndicatorType"
    |   RName (XmRInitialState           _) = "InitialState"
    |   RName (XmRInt                    _) = "Int"
    |   RName (XmRKeySym                 _) = "KeySym"
    |   RName (XmRKeySymTable            _) = "KeySymTable"
    |   RName (XmRKeyboardFocusPolicy    _) = "KeyboardFocusPolicy"
    |   RName (XmRLabelType              _) = "LabelType"
    |   RName (XmRMultiClick             _) = "MultiClick"
    |   RName (XmRNavigationType         _) = "NavigationType"
    |   RName (XmROrientation            _) = "Orientation"
    |   RName (XmRPacking                _) = "Packing"
    |   RName (XmRProcessingDirection    _) = "ProcessingDirection"
    |   RName (XmRResizePolicy           _) = "ResizePolicy"
    |   RName (XmRRowColumnType          _) = "RowColumnType"
    |   RName (XmRPixel                  _) = "Pixel"
    |   RName (XmRPixmap                 _) = "Pixmap"
    |   RName (XmRPosition               _) = "Position"
    |   RName (XmRScrollBarDisplayPolicy _) = "ScrollBarDisplayPolicy"
    |   RName (XmRScrollBarPlacement     _) = "ScrollBarPlacement"
    |   RName (XmRScrollingPolicy        _) = "ScrollingPolicy"
    |   RName (XmRSelectionPolicy        _) = "SelectionPolicy"
    |   RName (XmRShadowType             _) = "ShadowType"
    |   RName (XmRShort                  _) = "Short"
    |   RName (XmRStringDirection        _) = "StringDirection"
    |   RName (XmRString                 _) = "String"
    |   RName (XmRStringTable            _) = "StringTable"
    |   RName (XmRTextScanTypeTable      _) = "TextScanTypeTable"
    |   RName (XmRTranslationTable       _) = "TranslationTable"
    |   RName (XmRUnitType               _) = "UnitType"
    |   RName (XmRVisual                 _) = "Visual"
    |   RName (XmRVisualPolicy           _) = "VisualPolicy"
    |   RName (XmRWidget                 _) = "Widget"
    |   RName (XmRWidgetList             _) = "WidgetList"
    |   RName (XmRXmString               _) = "XmString"
    |   RName (XmRXmStringTable          _) = "XmStringTable";

    (* Added this function to remove the dependency on datatype representation.
       DCJM 28/3/01. *)
    fun insertValue (XmRAcceleratorTable       _) v = XmRAcceleratorTable (Cast v)
      | insertValue (XmRAlignment              _) v = XmRAlignment (Cast v)
      | insertValue (XmRArrowDirection         _) v = XmRArrowDirection (Cast v)
      | insertValue (XmRAtom                   _) v = XmRAtom (Cast v)
      | insertValue (XmRAttachment             _) v = XmRAttachment (Cast v)
      | insertValue (XmRBool                   _) v = XmRBool (Cast v)
      | insertValue (XmRButtonTypeTable        _) v = XmRButtonTypeTable (Cast v)
      | insertValue (XmRCardinal               _) v = XmRCardinal (Cast v)
      | insertValue (XmRColormap               _) v = XmRColormap (Cast v)
      | insertValue (XmRCommandWindowLocation  _) v = XmRCommandWindowLocation (Cast v)
      | insertValue (XmRCursor                 _) v = XmRCursor (Cast v)
      | insertValue (XmRDefaultButtonType      _) v = XmRDefaultButtonType (Cast v)
      | insertValue (XmRDeleteResponse         _) v = XmRDeleteResponse (Cast v)
      | insertValue (XmRDialogStyle            _) v = XmRDialogStyle (Cast v)
      | insertValue (XmRDialogType             _) v = XmRDialogType (Cast v)
      | insertValue (XmRDimension              _) v = XmRDimension (Cast v)
      | insertValue (XmREditMode               _) v = XmREditMode (Cast v)
      | insertValue (XmRFileTypeMask           _) v = XmRFileTypeMask (Cast v)
      | insertValue (XmRFontList               _) v = XmRFontList (Cast v)
      | insertValue (XmRIndicatorType          _) v = XmRIndicatorType (Cast v)
      | insertValue (XmRInitialState           _) v = XmRInitialState (Cast v)
      | insertValue (XmRInt                    _) v = XmRInt (Cast v)
      | insertValue (XmRKeySym                 _) v = XmRKeySym (Cast v)
      | insertValue (XmRKeySymTable            _) v = XmRKeySymTable (Cast v)
      | insertValue (XmRKeyboardFocusPolicy    _) v = XmRKeyboardFocusPolicy (Cast v)
      | insertValue (XmRLabelType              _) v = XmRLabelType (Cast v)
      | insertValue (XmRMultiClick             _) v = XmRMultiClick (Cast v)
      | insertValue (XmRNavigationType         _) v = XmRNavigationType (Cast v)
      | insertValue (XmROrientation            _) v = XmROrientation (Cast v)
      | insertValue (XmRPacking                _) v = XmRPacking (Cast v)
      | insertValue (XmRProcessingDirection    _) v = XmRProcessingDirection (Cast v)
      | insertValue (XmRResizePolicy           _) v = XmRResizePolicy (Cast v)
      | insertValue (XmRRowColumnType          _) v = XmRRowColumnType (Cast v)
      | insertValue (XmRPixel                  _) v = XmRPixel (Cast v)
      | insertValue (XmRPixmap                 _) v = XmRPixmap (Cast v)
      | insertValue (XmRPosition               _) v = XmRPosition (Cast v)
      | insertValue (XmRScrollBarDisplayPolicy _) v = XmRScrollBarDisplayPolicy (Cast v)
      | insertValue (XmRScrollBarPlacement     _) v = XmRScrollBarPlacement (Cast v)
      | insertValue (XmRScrollingPolicy        _) v = XmRScrollingPolicy (Cast v)
      | insertValue (XmRSelectionPolicy        _) v = XmRSelectionPolicy (Cast v)
      | insertValue (XmRShadowType             _) v = XmRShadowType (Cast v)
      | insertValue (XmRShort                  _) v = XmRShort (Cast v)
      | insertValue (XmRStringDirection        _) v = XmRStringDirection (Cast v)
      | insertValue (XmRString                 _) v = XmRString (Cast v)
      | insertValue (XmRStringTable            _) v = XmRStringTable (Cast v)
      | insertValue (XmRTextScanTypeTable      _) v = XmRTextScanTypeTable (Cast v)
      | insertValue (XmRTranslationTable       _) v = XmRTranslationTable (Cast v)
      | insertValue (XmRUnitType               _) v = XmRUnitType (Cast v)
      | insertValue (XmRVisual                 _) v = XmRVisual (Cast v)
      | insertValue (XmRVisualPolicy           _) v = XmRVisualPolicy (Cast v)
      | insertValue (XmRWidget                 _) v = XmRWidget (Cast v)
      | insertValue (XmRWidgetList             _) v = XmRWidgetList( Cast v)
      | insertValue (XmRXmString               _) v = XmRXmString (Cast v)
      | insertValue (XmRXmStringTable          _) v = XmRXmStringTable (Cast v);

    datatype Exn = EXN of unit ref * string * unit;
    
    val argTable = ref ([]:(Exn * string * XmRType) list);
    
    val Exn: Arg -> Exn = Cast;
    val Arg: Exn -> Arg = Cast;
    
    fun ArgName (e:Arg) =
    let
      (* open System; *)
      fun substring s i n = String.substring(s, i-1, n)
      
      val EXN (_,s,_) = Exn e;
      
      val n = size s;
    in
      if n < 3 orelse substring s 1 3 <> "XmN"
      then
        raise XWindows (s ^ " does not start with XmN")
      else
        substring s 4 (n - 3)
    end;
    
    fun FindByExn (e:Arg) =
    let
      val EXN (r1,n,value) = Exn e;

      (* This bit of code assumes that XmRType values are pairs with the
         tag in the second word and the value in the first.  That's no
         longer correct.  I've replaced it with the insertValue function
         above. DCJM 28/3/01. *)
      (*fun InsertValue (r:XmRType) =
      let
        val (_,tag) = Cast r;
      in
        Cast (value,tag):XmRType
      end;*)

      fun InsertValue (r:XmRType) = insertValue r value

      fun Find []     = raise XWindows (n ^ " has not been registered")
      |   Find (H::T) =
          let
            val (EXN (r2,_,_),s,r) = H;
          in
            if r1 = r2 then (s,InsertValue r) else Find T
          end;
    in
      Find (!argTable)
    end;
  
    fun FindByString s =
    let
      fun Find []     = raise XWindows (s ^ " has not been registered.")
      |   Find (H::T) =
          let
            val (_,t,_) = H;
          in
            if s = t then H else Find T
          end;
    in
      Find (!argTable)
    end;
  
    fun Length L =
    let
      fun Count []     n = n
      |   Count (_::T) n = Count T (n+1);
    in
      Count L 0
    end;
    
  in
    
    (*type CType = CType;*)
    
    val fromDefaultButtonType = fromDefaultButtonType;
    
    fun XtSetExnVals []     = []
    |   XtSetExnVals (H::T) =
        let
          val (s,r) = FindByExn H;
          
          val A = (s,CType r);
        in
          case H of
            XmNargv                   L => A :: XtSetExnVals (XmNargc                (Length L) :: T)
          | XmNbuttonAccelerators     L => A :: XtSetExnVals (XmNbuttonCount         (Length L) :: T)
          | XmNbuttonAcceleratorText  L => A :: XtSetExnVals (XmNbuttonCount         (Length L) :: T)
          | XmNbuttonMnemonicCharSets L => A :: XtSetExnVals (XmNbuttonCount         (Length L) :: T)
          | XmNbuttonMnemonics        L => A :: XtSetExnVals (XmNbuttonCount         (Length L) :: T)
          | XmNbuttons                L => A :: XtSetExnVals (XmNbuttonCount         (Length L) :: T)
          | XmNbuttonType             L => A :: XtSetExnVals (XmNbuttonCount         (Length L) :: T)
          | XmNchildren               L => A :: XtSetExnVals (XmNnumChildren         (Length L) :: T)
          | XmNdirListItems           L => A :: XtSetExnVals (XmNdirListItemCount    (Length L) :: T)
          | XmNfileListItems          L => A :: XtSetExnVals (XmNfileListItemCount   (Length L) :: T)
          | XmNhistoryItems           L => A :: XtSetExnVals (XmNhistoryItemCount    (Length L) :: T)
          | XmNitems                  L => A :: XtSetExnVals (XmNitemCount           (Length L) :: T)
          | XmNlistItems              L => A :: XtSetExnVals (XmNlistItemCount       (Length L) :: T)
          | XmNselectedItems          L => A :: XtSetExnVals (XmNselectedItemCount   (Length L) :: T)
          | XmNselectionArray         L => A :: XtSetExnVals (XmNselectionArrayCount (Length L) :: T)
          | _                           => A :: XtSetExnVals T
        end;

    fun XtSetExceptionType (E:'a -> Arg) (R:'a -> XmRType) =
    let
      val a:'a = Cast 0;
      
      val e = E a;
      val r = R a;
    in
      argTable := (Exn e,ArgName e,r) :: !argTable
    end;
    
    fun XtGetValue (w:Widget) (s:string):Arg =
    let
      val (EXN (u,n,_),_,r) = FindByString s;
    in
      Arg (EXN (u,n,Convert r (xcall (XCALL_GetValue,w,(s,CType r)))))
    end;

    local
      fun GetExns []           R C = (R,C)
      |   GetExns ((s,c,d)::T) R C =
      let
        val (EXN (u,n,_),_,r) = FindByString s;
        
        val convert = Convert r;
        
        fun MakeExn v = Arg (EXN (u,n,convert v));
      in
        GetExns T ((s,CType r,c,RName r,RName d,CType d) :: R) (MakeExn :: C)
      end;

      fun Zip []     _      acc = acc
      |   Zip _      []     acc = acc
      |   Zip (v::V) (c::C) acc = Zip V C (c v :: acc)

    in
        fun XtGetSubresources (w:Widget) (name:string) (class:string) (L:(string * string * XmRType) list):Arg list =
        let
          
          val (R,C) = GetExns L [] [];
          
          val V = xcall (XCALL_GetSubresources,w,name,class,R)
          
        in
          Zip V C []
        end;    
      
        fun XtGetApplicationResources (w:Widget) (L:(string * string * XmRType) list):Arg list =
        let
          val (R,C) = GetExns L [] [] ;
          val V = xcall (XCALL_XtGetApplicationResources,w,R)
        in
          Zip V C []
        end ;

    end;

    local
        (* These have been copied over from the XWindows module. *)
        infix 9 Or;
        fun a Or  b = xcall (12,a:int,b:int):int ;
        fun fromEventMask list =
        let
          fun MaskBit KeyPressMask             = 1
          |   MaskBit KeyReleaseMask           = 2
          |   MaskBit ButtonPressMask          = 4
          |   MaskBit ButtonReleaseMask        = 8
          |   MaskBit EnterWindowMask          = 16
          |   MaskBit LeaveWindowMask          = 32
          |   MaskBit PointerMotionMask        = 64
          |   MaskBit PointerMotionHintMask    = 128
          |   MaskBit Button1MotionMask        = 256
          |   MaskBit Button2MotionMask        = 512
          |   MaskBit Button3MotionMask        = 1024
          |   MaskBit Button4MotionMask        = 2048
          |   MaskBit Button5MotionMask        = 4096
          |   MaskBit ButtonMotionMask         = 8192
          |   MaskBit KeymapStateMask          = 16384
          |   MaskBit ExposureMask             = 32768
          |   MaskBit VisibilityChangeMask     = 65536
          |   MaskBit StructureNotifyMask      = 131072
          |   MaskBit ResizeRedirectMask       = 262144
          |   MaskBit SubstructureNotifyMask   = 524288
          |   MaskBit SubstructureRedirectMask = 1048576
          |   MaskBit FocusChangeMask          = 2097152
          |   MaskBit PropertyChangeMask       = 4194304
          |   MaskBit ColormapChangeMask       = 8388608
          |   MaskBit OwnerGrabButtonMask      = 16777216
          |   MaskBit ButtonClickMask          = 536870912 Or MaskBit ButtonPressMask Or MaskBit ButtonReleaseMask   (* extra event mask *) ;
      
          fun OrTogether []     acc = acc
          |   OrTogether (H::T) acc = OrTogether T (MaskBit H Or acc) ;
        in
          OrTogether list 0
        end
    in
        fun XtAddEventHandler (w: Widget) (ev: EventMask list) (nonmaskable: bool)
          (handler: Widget * 'a XEvent -> unit) : unit =
          xcall(XCALL_XtAddEventHandler, w, fromEventMask ev, nonmaskable, handler)
    end

  end (* local *);

end (* struct *);
