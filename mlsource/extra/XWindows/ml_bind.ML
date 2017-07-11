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

structure XWindows: XWINDOWS_SIG =
struct
local
  open XCall;
  open XAtoms;
    local
        val callX = RunCall.rtsCallFull1 "PolyXWindowsGeneral"
    in
        fun xcall n = RunCall.unsafeCast (callX (RunCall.unsafeCast n))
    end
in
  structure XAtoms   = XAtoms;
  structure XCursors = XCursors;
  structure XKeySyms = XKeySyms;

  exception XWindows = XWindows;

  (* abstypes *)

  abstype Colormap = Colormap with end;
  abstype Cursor   = Cursor   with end;
  abstype Display  = Display  with end;
  abstype Drawable = Drawable with end;
  abstype Font     = Font     with end;
  abstype GC       = GC       with end;
  abstype Visual   = Visual   with end;

  (* Geometry *)
  
  datatype XPoint = XPoint of { x:int,y:int };
  
  datatype XRectangle = R of { top:int,left:int,bottom:int,right:int };
  
  fun Left        (R {left,...})   = left;
  fun Right       (R {right,...})  = right;
  fun Top         (R {top,...})    = top;
  fun Bottom      (R {bottom,...}) = bottom;
  
  fun Width       (R {right,left,...}) = right  - left;
  fun Height      (R {top,bottom,...}) = bottom - top;
  
  fun TopLeft     (R {top,left,...})     = XPoint { x=left ,y=top    };
  fun TopRight    (R {top,right,...})    = XPoint { x=right,y=top    };
  fun BottomLeft  (R {bottom,left,...})  = XPoint { x=left ,y=bottom };
  fun BottomRight (R {bottom,right,...}) = XPoint { x=right,y=bottom };

  exception XRectangle of { top:int,left:int,bottom:int,right:int };
  
  fun Rect (rect as {left,top,right,bottom}) =
  (
    if left <= right andalso top <= bottom then R rect else raise XRectangle rect
  );

  fun Area {x,y,w,h} = Rect { left = x, top = y, right = x+w, bottom = y+h };
  
  fun DestructRect (R rect) = rect;

  fun DestructArea rect =
  let 
    val {left,top,right,bottom} = DestructRect rect;
  in
    { x = left, y = top, w = right-left, h = bottom-top }
  end;
  
  fun SplitRect rect =
  let
    val { top,left,bottom,right } = DestructRect rect;
  in
    (XPoint { x = left,  y = top },
     XPoint { x = right, y = bottom })
  end;
  
  fun min a b : int = if a < b then a else b;
  fun max a b : int = if a > b then a else b;
    
  fun MakeRect p1 p2 =
  let
    val (XPoint{x=x1,y=y1}) = p1;
    val (XPoint{x=x2,y=y2}) = p2;
  in
    Rect { left   = min x1 x2,
           top    = min y1 y2,
           right  = max x1 x2,
           bottom = max y1 y2 }
  end;
  
  fun Reflect rect =
  let
    val {top,left,bottom,right} = DestructRect rect;
  in
    Rect { left   = top,
           top    = left,
           right  = bottom,
           bottom = right }
  end;

  fun NegativePoint (XPoint{x,y}) = XPoint { x = ~x, y = ~y };

  datatype Section = Nothing | Section of XRectangle;
 
  fun Intersection r1 r2 =
  let
    val ML = max (Left r1)   (Left r2);
    val MT = max (Top r1)    (Top r2);
    val MR = min (Right r1)  (Right r2);
    val MB = min (Bottom r1) (Bottom r2);
  in
    if MR > ML andalso MB > MT then
      Section (Rect { left = ML, top = MT, right = MR, bottom = MB })
    else
      Nothing
  end;
    
  fun Union r1 r2 =
  let
    val ML = min (Left r1)   (Left r2);
    val MT = min (Top r1)    (Top r2);
    val MR = max (Right r1)  (Right r2);
    val MB = max (Bottom r1) (Bottom r2);
  in
    Rect { left = ML, top = MT, right = MR, bottom = MB }
  end;
  
  fun OutsetRect dist rect =
  let
    val { top,left,bottom,right } = DestructRect rect;
  in
    Rect { left   = left   - dist,
           top    = top    - dist,
           right  = right  + dist,
           bottom = bottom + dist } 
  end;
    
  fun OffsetRect rect (XPoint{x,y}) =
  let
    val { top,left,bottom,right } = DestructRect rect;
  in
    Rect { left   = left   + x,
           top    = top    + y,
           right  = right  + x,
           bottom = bottom + y } 
  end;
  
  fun IncludePoint (XPoint{x,y}) rect =
  let
    val { top,left,bottom,right } = DestructRect rect;
  in
    Rect { left   = min x left,
           top    = min y top,
           right  = max x right,
           bottom = max y bottom }
  end;
  
  infix AddPoint SubtractPoint;

  fun (XPoint{x,y}) AddPoint (XPoint{x=dx,y=dy}) =
  (
    XPoint { x = x + dx, y = y + dy }
  );
  
  fun (XPoint{x,y}) SubtractPoint (XPoint{x=dx,y=dy}) =
  (
    XPoint { x = x - dx, y = y - dy }
  );
  
  infix Inside
        Overlap
        Within
        LeftOf
        RightOf
        AboveOf
        BelowOf
        HorizontallyAbutting
        VerticallyAbutting;
  
  fun r1 Inside r2 =
  (
    Left   r1 >= Left   r2 andalso
    Right  r1 <= Right  r2 andalso
    Top    r1 >= Top    r2 andalso
    Bottom r1 <= Bottom r2
  );

  fun r1 Overlap r2 = (case (Intersection r1 r2) of Nothing => false | Section _ => true);

  fun (XPoint{x,y}) Within rect =
  (
     x >= Left   rect andalso
     y >= Top    rect andalso
     x <  Right  rect andalso
     y <  Bottom rect
  );
  
  fun (XPoint{x,y}) LeftOf  rect = (x <  Left   rect);
  fun (XPoint{x,y}) RightOf rect = (x >= Right  rect);
  fun (XPoint{x,y}) AboveOf rect = (y <  Top    rect);
  fun (XPoint{x,y}) BelowOf rect = (y >= Bottom rect);
  
  fun a HorizontallyAbutting b = 
  (
    (Right a = Left b orelse Left a = Right b)
    andalso (Top    a <= Top b)
    andalso (Bottom a >= Bottom b)
  );
  
  fun a VerticallyAbutting b = 
  (
    (Bottom a = Top b orelse Top a = Bottom b)
    andalso (Left  a <= Left b)
    andalso (Right a >= Right b)
  );
  
  val origin = XPoint { x=0,y=0 };

  val empty = Area {x=0,y=0,w=0,h=0}; val a1 = Area { x=0,y=0,w=1,h=1 };

  (* Colorcells 100 *)
  
  datatype XColor = XColor of { doRed:   bool,
                                doGreen: bool,
                                doBlue:  bool,
                                red:     int,
                                green:   int,
                                blue:    int,
                                pixel:   int };
  
  fun Pixel (XColor {pixel,...}) = pixel;
  
  fun RGB (XColor {red,green,blue,...}) = (red,green,blue);
  
  fun Not (a:int):int = xcall (XCALL_Not,a);

  infix 9 And Or Xor >> <<;

  fun a And b = xcall (XCALL_And,a:int,b:int):int;
  fun a Or  b = xcall (XCALL_Or,a:int,b:int):int;
  fun a Xor b = xcall (XCALL_Xor,a:int,b:int):int;
  fun a >>  b = xcall (XCALL_DownShift,a:int,b:int):int;
  fun a <<  b = xcall (XCALL_UpShift,a:int,b:int):int;
  
  fun XAllocColor (cmap:Colormap) (x:XColor):XColor
    = xcall (XCALL_XAllocColor,cmap,x);

  fun XAllocColorCells (cmap:Colormap) (contig:bool) (nplanes:int) (ncolors:int):(int list * int list)
    = xcall (XCALL_XAllocColorCells,cmap,contig,nplanes,ncolors);

  fun XAllocColorPlanes (cmap:Colormap) (contig:bool) (ncolors:int) (nreds:int) (ngreens:int) (nblues:int):(int list * int * int * int) =
  (
    xcall (XCALL_XAllocColorPlanes,cmap,contig,ncolors,nreds,ngreens,nblues)
  );

  fun XAllocNamedColor (cmap:Colormap) (name:string):(XColor * XColor) =
    xcall (XCALL_XAllocNamedColor,cmap,name);
  
  fun XFreeColors (cmap:Colormap) (pixels:int list) (planes:int):unit =
    xcall (XCALL_XFreeColors,cmap,pixels,planes);

  fun XLookupColor (cmap:Colormap) (name:string):(XColor * XColor) =
     xcall (XCALL_XLookupColor,cmap,name);

  fun XParseColor (cmap:Colormap) (name:string):XColor =
    xcall (XCALL_XParseColor,cmap,name);

  fun XQueryColor (cmap:Colormap) (pixel:int):XColor =
    xcall (XCALL_XQueryColor,cmap,pixel);

  fun XQueryColors (cmap:Colormap) (pixels:int list):XColor list =
    xcall (XCALL_XQueryColors,cmap,pixels);

  fun XStoreColor (cmap:Colormap) (x:XColor):unit =
    xcall (XCALL_XStoreColor,cmap,x);
  
  fun XStoreColors (cmap:Colormap) (L:XColor list):unit =
    xcall (XCALL_XStoreColors,cmap,L);

  fun XStoreNamedColor (cmap:Colormap) (name:string) (pixel:int) (r:bool,g:bool,b:bool):unit =
    xcall (XCALL_XStoreNamedColor,cmap,name,pixel,r,g,b);

  fun BlackPixel (d:Display) = xcall (XCALL_BlackPixel,d);
  fun WhitePixel (d:Display) = xcall (XCALL_WhitePixel,d);

  (* Colormaps 150 *)
  
  datatype AllocType = AllocNone | AllocAll;

  fun fromAllocType AllocNone = 0
  |   fromAllocType AllocAll  = 1;

  datatype XStandardColormap = XStandardColormap of { colormap:  Colormap,
                                                      redMax:    int,
                                                      redMult:   int,
                                                      greenMax:  int,
                                                      greenMult: int,
                                                      blueMax:   int,
                                                      blueMult:  int,
                                                      basePixel: int };

  fun XCopyColormapAndFree (cmap:Colormap):Colormap =
    xcall (XCALL_XCopyColormapAndFree,cmap);

  fun XCreateColormap (d:Drawable) (v:Visual) (a:AllocType):Colormap =
    xcall (XCALL_XCreateColormap,d,v,fromAllocType a);

  fun XInstallColormap (cmap:Colormap):unit =
    xcall (XCALL_XInstallColormap,cmap);

  fun XListInstalledColormaps (d:Drawable):Colormap list =
    xcall (XCALL_XListInstalledColormaps,d);

  fun XUninstallColormap (cmap:Colormap):unit =
    xcall (XCALL_XUninstallColormap,cmap);

  fun DefaultColormap (d:Display) = xcall (XCALL_DefaultColormap,d) : Colormap;
  fun DefaultVisual (d:Display)   = xcall (XCALL_DefaultVisual,d) : Visual;
  fun DisplayCells (d:Display)    = xcall (XCALL_DisplayCells,d) : int;
  
  datatype VisualClass = 
     StaticGray
   | GrayScale
   | StaticColor
   | PseudoColor
   | TrueColor
   | DirectColor;
  
  fun VisualClass (v:Visual) =
    case xcall (XCALL_VisualClass,v) of
      0 => StaticGray
    | 1 => GrayScale
    | 2 => StaticColor
    | 3 => PseudoColor
    | 4 => TrueColor
    | _ => DirectColor;

  fun VisualRedMask   (v:Visual):int = xcall (XCALL_VisualRedMask,v);
  fun VisualGreenMask (v:Visual):int = xcall (XCALL_VisualGreenMask,v);
  fun VisualBlueMask  (v:Visual):int = xcall (XCALL_VisualBlueMask,v);
  
  (* Cursors 200 *)

  fun XCreateFontCursor (d:Display) (n:int):Cursor =
    xcall (XCALL_XCreateFontCursor,d,n);

  fun XCreateGlyphCursor (sf:Font) (mf:Font) (sc:int) (mc:int) (fg:XColor) (bg:XColor):Cursor =
    xcall (XCALL_XCreateGlyphCursor,sf,mf,sc,mc,fg,bg);

  fun XCreatePixmapCursor (src:Drawable)
                          (mask:Drawable)
                          (fg:XColor)
                          (bg:XColor)
                          (hotspot:XPoint):Cursor =
    xcall (XCALL_XCreatePixmapCursor,src,mask,fg,bg,hotspot);

  fun XDefineCursor (d:Drawable) (c:Cursor):unit =
    xcall(XCALL_XDefineCursor,d,c);

  fun XQueryBestCursor (d:Drawable) (r:XRectangle):XRectangle =
    xcall (XCALL_XQueryBestCursor,d,r);

  fun XRecolorCursor (c:Cursor) (fg:XColor) (bg:XColor) =
    xcall (XCALL_XRecolorCursor,c,fg,bg);
  
  fun XUndefineCursor (d:Drawable):unit =
    xcall (XCALL_XUndefineCursor,d);

  (* Display Specifications 250 *)
  
  datatype BackingStore = NotUseful | WhenMapped | Always;
  
  fun toBackingStore 0 = NotUseful
  |   toBackingStore 1 = WhenMapped
  |   toBackingStore _ = Always;
  
  fun fromBackingStore NotUseful  = 0
  |   fromBackingStore WhenMapped = 1
  |   fromBackingStore Always     = 2;
  
  datatype EventMask = 
    KeyPressMask             | KeyReleaseMask      | ButtonPressMask     | ButtonReleaseMask
  | EnterWindowMask          | LeaveWindowMask     | PointerMotionMask   | PointerMotionHintMask
  | Button1MotionMask        | Button2MotionMask   | Button3MotionMask   | Button4MotionMask        
  | Button5MotionMask        | ButtonMotionMask    | KeymapStateMask     | ExposureMask
  | VisibilityChangeMask     | StructureNotifyMask | ResizeRedirectMask  | SubstructureNotifyMask
  | SubstructureRedirectMask | FocusChangeMask     | PropertyChangeMask  | ColormapChangeMask
  | OwnerGrabButtonMask
  | ButtonClickMask      (* extra event mask *);

  exception BadEventMask of int;

  fun toEventMask n =
  let
    fun MaskName 1          = KeyPressMask
    |   MaskName 2          = KeyReleaseMask
    |   MaskName 4          = ButtonPressMask
    |   MaskName 8          = ButtonReleaseMask
    |   MaskName 16         = EnterWindowMask
    |   MaskName 32         = LeaveWindowMask
    |   MaskName 64         = PointerMotionMask
    |   MaskName 128        = PointerMotionHintMask
    |   MaskName 256        = Button1MotionMask
    |   MaskName 512        = Button2MotionMask
    |   MaskName 1024       = Button3MotionMask
    |   MaskName 2048       = Button4MotionMask
    |   MaskName 4096       = Button5MotionMask
    |   MaskName 8192       = ButtonMotionMask
    |   MaskName 16384      = KeymapStateMask
    |   MaskName 32768      = ExposureMask
    |   MaskName 65536      = VisibilityChangeMask
    |   MaskName 131072     = StructureNotifyMask
    |   MaskName 262144     = ResizeRedirectMask
    |   MaskName 524288     = SubstructureNotifyMask
    |   MaskName 1048576    = SubstructureRedirectMask
    |   MaskName 2097152    = FocusChangeMask
    |   MaskName 4194304    = PropertyChangeMask
    |   MaskName 8388608    = ColormapChangeMask
    |   MaskName 16777216   = OwnerGrabButtonMask
    |   MaskName 536870912  = ButtonClickMask          (* extra event mask *)
    |   MaskName n          = raise BadEventMask n;
  
    fun ShiftAndMask 0 mask acc = acc
    |   ShiftAndMask n mask acc = ShiftAndMask (n And Not mask) (mask << 1) (if (n And mask) <> 0 then MaskName mask :: acc else acc);
  in
    ShiftAndMask n 1 []
  end;
  
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
    |   MaskBit ButtonClickMask          = 536870912 Or MaskBit ButtonPressMask Or MaskBit ButtonReleaseMask   (* extra event mask *);
  
    fun OrTogether []     acc = acc
    |   OrTogether (H::T) acc = OrTogether T (MaskBit H Or acc);
  in
    OrTogether list 0
  end;
  
  val AllPlanes = Not 0;

  fun CellsOfScreen (d:Display)   = xcall (XCALL_CellsOfScreen,d) : int;
  fun DefaultDepth (d:Display)    = xcall (XCALL_DefaultDepth,d) : int;
  fun DisplayHeight (d:Display)   = xcall (XCALL_DisplayHeight,d) : int;
  fun DisplayHeightMM (d:Display) = xcall (XCALL_DisplayHeightMM,d) : int;
  fun DisplayPlanes (d:Display)   = xcall (XCALL_DisplayPlanes,d) : int;
  fun DisplayString (d:Display)   = xcall (XCALL_DisplayString,d) : string;
  fun DisplayWidth (d:Display)    = xcall (XCALL_DisplayWidth,d) : int;
  fun DisplayWidthMM (d:Display)  = xcall (XCALL_DisplayWidthMM,d) : int;

  fun DoesBackingStore (d:Display) = 
    toBackingStore (xcall (XCALL_DoesBackingStore,d));

  fun DoesSaveUnders (d:Display) =
    xcall (XCALL_DoesSaveUnders,d) : bool;
  
  fun EventMaskOfScreen (d:Display) =
    toEventMask (xcall (XCALL_EventMaskOfScreen,d));
  
  fun MaxCmapsOfScreen (d:Display)  = xcall (XCALL_MaxCmapsOfScreen,d) : int;
  fun MinCmapsOfScreen (d:Display)  = xcall (XCALL_MinCmapsOfScreen,d) : int;
  fun ProtocolRevision (d:Display)  = xcall (XCALL_ProtocolRevision,d) : int;
  fun ProtocolVersion (d:Display)   = xcall (XCALL_ProtocolVersion,d) : int;
  fun ServerVendor (d:Display)      = xcall (XCALL_ServerVendor,d) : string;
  fun VendorRelease (d:Display)     = xcall (XCALL_VendorRelease,d) : int;

  (* Drawing Primitives 300 *)
  
  datatype XArc = XArc of XRectangle * int * int;
  
  datatype XTextItem   = XTextItem   of string   * int * Font;
  datatype XTextItem16 = XTextItem16 of int list * int * Font;
  
  datatype CoordMode = CoordModeOrigin | CoordModePrevious;
  
  fun fromCoordMode CoordModeOrigin   = 0
  |   fromCoordMode CoordModePrevious = 1;

  datatype PolyShape = Complex | Nonconvex | Convex;
  
  fun fromPolyShape Complex   = 0
  |   fromPolyShape Nonconvex = 1
  |   fromPolyShape Convex    = 2;

  fun XClearArea (d:Drawable) (r:XRectangle) (e:bool):unit =
    xcall (XCALL_XClearArea,d,r,e);

  fun XClearWindow (d:Drawable):unit =
    xcall (XCALL_XClearWindow,d);

  fun XCopyArea (src:Drawable) (dst:Drawable) (gc:GC) (P:XPoint) (r:XRectangle):unit = 
    xcall (XCALL_XCopyArea,src,dst,gc,P,r);

  fun XCopyPlane (src:Drawable) (dst:Drawable) (gc:GC) (P:XPoint) (r:XRectangle) (plane:int):unit =
     xcall (XCALL_XCopyPlane,src,dst,gc,P,r,plane);

  fun XDrawArc (d:Drawable) (gc:GC) (a:XArc):unit =
    xcall (XCALL_XDrawArc,d,gc,a);

  fun XDrawArcs (d:Drawable) (gc:GC) (L:XArc list):unit =
    xcall (XCALL_XDrawArcs,d,gc,L);

  fun XDrawImageString (d:Drawable) (gc:GC) (p:XPoint) (s:string):unit =
    xcall (XCALL_XDrawImageString,d,gc,p,s);

  fun XDrawImageString16 (d:Drawable) (gc:GC) (p:XPoint) (L:int list):unit =
    xcall (XCALL_XDrawImageString16,d,gc,p,L);

  fun XDrawLine (d:Drawable) (gc:GC) (p1:XPoint) (p2:XPoint):unit =
    xcall (XCALL_XDrawLine,d,gc,p1,p2);

  fun XDrawLines (d:Drawable) (gc:GC) (L:XPoint list) (mode:CoordMode):unit =
    xcall (XCALL_XDrawLines,d,gc,L,fromCoordMode mode);

  fun XDrawPoint (d:Drawable) (gc:GC) (p:XPoint):unit =
    xcall (XCALL_XDrawPoint,d,gc,p);

  fun XDrawPoints (d:Drawable) (gc:GC) (L:XPoint list) (mode:CoordMode):unit =
    xcall (XCALL_XDrawPoints,d,gc,L,fromCoordMode mode);

  fun XDrawRectangle (d:Drawable) (gc:GC) (r:XRectangle):unit =
    xcall (XCALL_XDrawRectangle,d,gc,r);

  fun XDrawRectangles (d:Drawable) (gc:GC) (L:XRectangle list):unit =
    xcall (XCALL_XDrawRectangles,d,gc,L);

  fun XDrawSegments (d:Drawable) (gc:GC) (L:(XPoint * XPoint) list):unit =
    xcall (XCALL_XDrawSegments,d,gc,L);

  fun XDrawString (d:Drawable) (gc:GC) (p:XPoint) (s:string):unit =
    xcall (XCALL_XDrawString,d,gc,p,s);

  fun XDrawString16 (d:Drawable) (gc:GC) (p:XPoint) (s:int list):unit =
    xcall (XCALL_XDrawString16,d,gc,p,s);

  fun XDrawText (d:Drawable) (gc:GC) (p:XPoint) (L:XTextItem list):unit =
    xcall (XCALL_XDrawText,d,gc,p,L);

  fun XDrawText16 (d:Drawable) (gc:GC) (p:XPoint) (L:XTextItem16 list):unit =
  xcall (XCALL_XDrawText16,d,gc,p,L);

  fun XFillArc (d:Drawable) (gc:GC) (a:XArc):unit =
    xcall (XCALL_XFillArc,d,gc,a);

  fun XFillArcs (d:Drawable) (gc:GC) (L:XArc list):unit =
    xcall (XCALL_XFillArcs,d,gc,L);

  fun XFillPolygon (d:Drawable) (gc:GC) (L:XPoint list) (shape:PolyShape) (mode:CoordMode):unit =
    xcall (XCALL_XFillPolygon,d,gc,L,fromPolyShape shape,fromCoordMode mode);

  fun XFillRectangle (d:Drawable) (gc:GC) (r:XRectangle):unit =
    xcall (XCALL_XFillRectangle,d,gc,r);

  fun XFillRectangles (d:Drawable) (gc:GC) (L:XRectangle list):unit =
    xcall (XCALL_XFillRectangles,d,gc,L);

  (* Events 350 *)
  
  datatype Modifier =
    ShiftMask   | LockMask    | ControlMask
  | Mod1Mask    | Mod2Mask    | Mod3Mask    | Mod4Mask    | Mod5Mask
  | Button1Mask | Button2Mask | Button3Mask | Button4Mask | Button5Mask
  | AnyModifier;

  exception BadModifier of int;
  
  fun toModifier n =
  let
    fun MaskName 1          = ShiftMask
    |   MaskName 2          = LockMask
    |   MaskName 4          = ControlMask
    |   MaskName 8          = Mod1Mask
    |   MaskName 16         = Mod2Mask
    |   MaskName 32         = Mod3Mask
    |   MaskName 64         = Mod4Mask
    |   MaskName 128        = Mod5Mask
    |   MaskName 256        = Button1Mask
    |   MaskName 512        = Button2Mask
    |   MaskName 1024       = Button3Mask
    |   MaskName 2048       = Button4Mask
    |   MaskName 4096       = Button5Mask
    |   MaskName 32768      = AnyModifier
    |   MaskName n          = raise BadModifier n;
  
    fun ShiftAndMask 0 mask acc = acc
    |   ShiftAndMask n mask acc = ShiftAndMask (n And Not mask) (mask << 1) (if (n And mask) <> 0 then MaskName mask :: acc else acc);
  in
    ShiftAndMask n 1 []
  end;
  
  fun fromModifier list =
  let
    fun MaskBit ShiftMask   = 1
    |   MaskBit LockMask    = 2
    |   MaskBit ControlMask = 4
    |   MaskBit Mod1Mask    = 8
    |   MaskBit Mod2Mask    = 16
    |   MaskBit Mod3Mask    = 32
    |   MaskBit Mod4Mask    = 64
    |   MaskBit Mod5Mask    = 128
    |   MaskBit Button1Mask = 256
    |   MaskBit Button2Mask = 512
    |   MaskBit Button3Mask = 1024
    |   MaskBit Button4Mask = 2048
    |   MaskBit Button5Mask = 4096
    |   MaskBit AnyModifier = 32768;

    fun OrTogether []     acc = acc
    |   OrTogether (H::T) acc = OrTogether T (MaskBit H Or acc);
  in
    OrTogether list 0
  end;
    
  datatype ButtonName = Button1 | Button2 | Button3 | Button4 | Button5 | AnyButton;
  
  fun toButtonName 1 = Button1
  |   toButtonName 2 = Button2
  |   toButtonName 3 = Button3
  |   toButtonName 4 = Button4
  |   toButtonName 5 = Button5
  |   toButtonName _ = AnyButton;
  
  fun fromButtonName Button1   = 1
  |   fromButtonName Button2   = 2
  |   fromButtonName Button3   = 3
  |   fromButtonName Button4   = 4
  |   fromButtonName Button5   = 5
  |   fromButtonName AnyButton = 0;
  
  datatype Placement = PlaceOnTop | PlaceOnBottom;
  
  fun toPlacement 0 = PlaceOnTop
  |   toPlacement _ = PlaceOnBottom;

  datatype StackMode = Above | Below | TopIf | BottomIf | Opposite;
  
  fun toStackMode 0 = Above
  |   toStackMode 1 = Below
  |   toStackMode 2 = TopIf
  |   toStackMode 3 = BottomIf
  |   toStackMode _ = Opposite;
  
  fun fromStackMode Above    = 0
  |   fromStackMode Below    = 1
  |   fromStackMode TopIf    = 2
  |   fromStackMode BottomIf = 3
  |   fromStackMode Opposite = 4;
  
  datatype NotifyMode = NotifyNormal | NotifyGrab | NotifyUngrab | NotifyWhileGrabbed;
  
  fun toNotifyMode 0 = NotifyNormal
  |   toNotifyMode 1 = NotifyGrab
  |   toNotifyMode 2 = NotifyUngrab
  |   toNotifyMode _ = NotifyWhileGrabbed;
  
  datatype NotifyDetail = NotifyAncestor         | NotifyVirtual | NotifyInferior    | NotifyNonLinear
                        | NotifyNonLinearVirtual | NotifyPointer | NotifyPointerRoot | NotifyDetailNone;
  
  fun toNotifyDetail 0 = NotifyAncestor
  |   toNotifyDetail 1 = NotifyVirtual
  |   toNotifyDetail 2 = NotifyInferior
  |   toNotifyDetail 3 = NotifyNonLinear
  |   toNotifyDetail 4 = NotifyNonLinearVirtual
  |   toNotifyDetail 5 = NotifyPointer
  |   toNotifyDetail 6 = NotifyPointerRoot
  |   toNotifyDetail _ = NotifyDetailNone;
  
  datatype GraphicsCode = CopyArea | CopyPlane;
  
  fun toGraphicsCode 62 = CopyArea
  |   toGraphicsCode _  = CopyPlane;
  
  datatype Visibility = VisibilityUnobscured | VisibilityPartiallyObscured | VisibilityFullyObscured;
  
  fun toVisibility 0 = VisibilityUnobscured
  |   toVisibility 1 = VisibilityPartiallyObscured
  |   toVisibility _ = VisibilityFullyObscured;
  
  datatype 'a XEvent = ButtonPress      of { sendEvent:   bool,
                                             window:      Drawable,
                                             root:        Drawable,
                                             subwindow:   Drawable,
                                             time:        int,
                                             pointer:     XPoint,
                                             rootPointer: XPoint,
                                             modifiers:   Modifier list,
                                             button:      ButtonName }

                     | ButtonRelease    of { sendEvent:   bool,
                                             window:      Drawable,
                                             root:        Drawable,
                                             subwindow:   Drawable,
                                             time:        int,
                                             pointer:     XPoint,
                                             rootPointer: XPoint,
                                             modifiers:   Modifier list,
                                             button:      ButtonName }

                     | ButtonClick      of { sendEvent:   bool,          (* extra event type *)
                                             window:      Drawable,
                                             root:        Drawable,
                                             subwindow:   Drawable,
                                             time:        int,
                                             pointer:     XPoint,
                                             rootPointer: XPoint,
                                             modifiers:   Modifier list,
                                             button:      ButtonName,
                                             up:          int,           (* number of up   transitions *)
                                             down:        int }          (* number of down transitions *)

                     | CirculateNotify  of { sendEvent: bool,
                                             event:     Drawable,
                                             window:    Drawable,
                                             place:     Placement }

                     | CirculateRequest of { sendEvent: bool,
                                             parent:    Drawable,
                                             window:    Drawable,
                                             place:     Placement }

                     | ColormapNotify   of { sendEvent: bool,
                                             window:    Drawable,
                                             colormap:  Colormap,
                                             new:       bool,
                                             installed: bool }

                     | ConfigureNotify  of { sendEvent:        bool,
                                             event:            Drawable,
                                             window:           Drawable,
                                             position:         XPoint,
                                             size:             XRectangle,
                                             borderWidth:      int,
                                             above:            Drawable,
                                             overrideRedirect: bool }

                     | ConfigureRequest of { sendEvent:   bool,
                                             parent:      Drawable,
                                             window:      Drawable,
                                             position:    XPoint,
                                             size:        XRectangle,
                                             borderWidth: int,
                                             above:       Drawable,
                                             detail:      StackMode }

                     | CreateNotify     of { sendEvent:        bool,
                                             parent:           Drawable,
                                             window:           Drawable,
                                             position:         XPoint,
                                             size:             XRectangle,
                                             borderWidth:      int,
                                             overrideRedirect: bool }

                     | DestroyNotify    of { sendEvent: bool,
                                             event:     Drawable,
                                             window:    Drawable }

                     | EnterNotify      of { sendEvent:   bool,
                                             window:      Drawable,
                                             root:        Drawable,
                                             subwindow:   Drawable,
                                             time:        int,
                                             pointer:     XPoint,
                                             rootPointer: XPoint,
                                             mode:        NotifyMode,
                                             detail:      NotifyDetail,
                                             focus:       bool,
                                             modifiers:   Modifier list }

                     | LeaveNotify      of { sendEvent:   bool,
                                             window:      Drawable,
                                             root:        Drawable,
                                             subwindow:   Drawable,
                                             time:        int,
                                             pointer:     XPoint,
                                             rootPointer: XPoint,
                                             mode:        NotifyMode,
                                             detail:      NotifyDetail,
                                             focus:       bool,
                                             modifiers:   Modifier list }

                     | Expose           of { sendEvent: bool,
                                             window:    Drawable,
                                             region:    XRectangle,
                                             count:     int }

                     | FocusIn          of { sendEvent: bool,
                                             window:    Drawable,
                                             mode:      NotifyMode,
                                             detail:    NotifyDetail }

                     | FocusOut         of { sendEvent: bool,
                                             window:    Drawable,
                                             mode:      NotifyMode,
                                             detail:    NotifyDetail }

                     | GraphicsExpose   of { sendEvent: bool,
                                             window:    Drawable,
                                             region:    XRectangle,
                                             count:     int,
                                             code:      GraphicsCode }

                     | NoExpose         of { sendEvent: bool,
                                             window:    Drawable,
                                             code:      GraphicsCode }

                     | GravityNotify    of { sendEvent: bool,
                                             event:     Drawable,
                                             window:    Drawable,
                                             position:  XPoint }

                     | KeymapNotify     of { sendEvent: bool,
                                             window:    Drawable,
                                             keyVector: bool list (* 256 bools *) }

                     | KeyPress         of { sendEvent:   bool,
                                             window:      Drawable,
                                             root:        Drawable,
                                             subwindow:   Drawable,
                                             time:        int,
                                             pointer:     XPoint,
                                             rootPointer: XPoint,
                                             modifiers:   Modifier list,
                                             keycode:     int }

                     | KeyRelease       of { sendEvent:   bool,
                                             window:      Drawable,
                                             root:        Drawable,
                                             subwindow:   Drawable,
                                             time:        int,
                                             pointer:     XPoint,
                                             rootPointer: XPoint,
                                             modifiers:   Modifier list,
                                             keycode:     int }

                     | MapNotify        of { sendEvent:        bool,
                                             event:            Drawable,
                                             window:           Drawable,
                                             overrideRedirect: bool }

                     | UnmapNotify      of { sendEvent:     bool,
                                             event:         Drawable,
                                             window:        Drawable,
                                             fromConfigure: bool }

                     | MapRequest       of { sendEvent: bool,
                                             parent:    Drawable,
                                             window:    Drawable }

                     | MotionNotify     of { sendEvent:   bool,
                                             window:      Drawable,
                                             root:        Drawable,
                                             subwindow:   Drawable,
                                             time:        int,
                                             pointer:     XPoint,
                                             rootPointer: XPoint,
                                             modifiers:   Modifier list,
                                             isHint:      bool }

                     | ReparentNotify   of { sendEvent:        bool,
                                             event:            Drawable,
                                             window:           Drawable,
                                             parent:           Drawable,
                                             position:         XPoint,
                                             overrideRedirect: bool }

                     | ResizeRequest    of { sendEvent: bool,
                                             window:    Drawable,
                                             size:      XRectangle }

                     | SelectionClear   of { sendEvent: bool,
                                             window:    Drawable,
                                             selection: int,
                                             time:      int }
                     
                     | SelectionNotify  of { sendEvent: bool,
                                             requestor: Drawable,
                                             selection: int,
                                             target:    int,
                                             property:  int,
                                             time:      int }
                     
                     | SelectionRequest of { sendEvent: bool,
                                             owner:     Drawable,
                                             requestor: Drawable,
                                             selection: int,
                                             target:    int,
                                             property:  int,
                                             time:      int }
                     
                     | VisibilityNotify of { sendEvent:  bool,
                                             window:     Drawable,
                                             visibility: Visibility }

                     | DeleteRequest    of { window: Drawable }              (* extra event type *)

                     | Message          of { window: Drawable, message: 'a } (* extra event type *)
                     
                     | NoEvent

  fun XSelectInput (d:Drawable) (m:EventMask list):unit =
    xcall (XCALL_XSelectInput,d,fromEventMask m);
  
  fun XSynchronize (d:Display) (n:int):unit =
    xcall (XCALL_XSynchronize,d,n);
  
  fun XSetHandler (drawable:Drawable) (Handler:'a XEvent * 'b -> 'b) (initialState:'b) =
  (
    xcall (XCALL_SetState,drawable,Handler,initialState);
    
    fn (ms:int) => fn (alpha:'a) =>
      xcall (XCALL_InsertTimeout,drawable,ms,alpha,Handler) 
  );
  
  fun NullHandler (e,s) = s;

  local
    fun Exists x = xcall (XCALL_ResourceExists,x):bool;
  in
    val ColormapExists = Exists : Colormap -> bool;
    val CursorExists   = Exists : Cursor   -> bool;
    val DrawableExists = Exists : Drawable -> bool;
    val FontExists     = Exists : Font     -> bool;
    val GCExists       = Exists : GC       -> bool;
    val VisualExists   = Exists : Visual   -> bool;
    val DisplayExists  = Exists : Display  -> bool;
  end;
  
  local
    fun GetDisplay x = xcall (XCALL_GetDisplay,x):Display;
  in
    val ColormapDisplay = GetDisplay : Colormap -> Display;
    val CursorDisplay   = GetDisplay : Cursor   -> Display;
    val DrawableDisplay = GetDisplay : Drawable -> Display;
    val FontDisplay     = GetDisplay : Font     -> Display;
    val GCDisplay       = GetDisplay : GC       -> Display;
    val VisualDisplay   = GetDisplay : Visual   -> Display;
  end;

  local

    fun MakeXKeyEvent (_,sendEvent,window,(root,subwindow,time,pointer,rootPointer,modifiers,keycode),_,_) =
    {
      sendEvent   = sendEvent,
      window      = window,
      root        = root,
      subwindow   = subwindow,
      time        = time,
      pointer     = pointer,
      rootPointer = rootPointer,
      modifiers   = toModifier modifiers,
      keycode     = keycode
    };
    
    fun MakeXButtonEvent (_,sendEvent,window,(root,subwindow,time,pointer,rootPointer,modifiers,button),_,_) =
    {
      sendEvent   = sendEvent,
      window      = window,
      root        = root,
      subwindow   = subwindow,
      time        = time,
      pointer     = pointer,
      rootPointer = rootPointer,
      modifiers   = toModifier modifiers,
      button      = toButtonName button
    };

    fun MakeXMotionEvent (_,sendEvent,window,(root,subwindow,time,pointer,rootPointer,modifiers,isHint),_,_) =
    {
      sendEvent   = sendEvent,
      window      = window,
      root        = root,
      subwindow   = subwindow,
      time        = time,
      pointer     = pointer,
      rootPointer = rootPointer,
      modifiers   = toModifier modifiers,
      isHint      = isHint
    };
    
    fun MakeXCrossingEvent (_,sendEvent,window,(root,subwindow,time,pointer,rootPointer,mode,detail,focus,modifiers),_,_) =
    {
      sendEvent   = sendEvent,
      window      = window,
      root        = root,
      subwindow   = subwindow,
      time        = time,
      pointer     = pointer,
      rootPointer = rootPointer,
      mode        = toNotifyMode   mode,
      detail      = toNotifyDetail detail,
      focus       = focus,
      modifiers   = toModifier modifiers
    };
    
    fun MakeXExposeEvent (_,sendEvent,window,(region,count),_,_) =
    {
      sendEvent = sendEvent,
      window    = window,
      region    = region,
      count     = count
    };
    
    fun MakeXGraphicsExposeEvent (_,sendEvent,window,(region,count,code),_,_) =
    {
      sendEvent = sendEvent,
      window    = window,
      region    = region,
      count     = count,
      code      = toGraphicsCode code
    };
    
    fun MakeXNoExposeEvent (_,sendEvent,window,code,_,_) =
    {
      sendEvent = sendEvent,
      window    = window,
      code      = toGraphicsCode code
    };
    
    fun MakeXVisibilityEvent (_,sendEvent,window,visibility,_,_) =
    {
      sendEvent  = sendEvent,
      window     = window,
      visibility = toVisibility visibility
    };
    
    fun MakeXCreateWindowEvent (_,sendEvent,parent,(window,position,size,borderWidth,overrideRedirect),_,_) =
    {
      sendEvent        = sendEvent,
      window           = window,
      parent           = parent,
      position         = position,
      size             = size,
      borderWidth      = borderWidth,
      overrideRedirect = overrideRedirect
    };
    
    fun MakeXDestroyWindowEvent (_,sendEvent,event,window,_,_) =
    {
      sendEvent = sendEvent,
      event     = event,
      window    = window
    };
    
    fun MakeXUnmapEvent (_,sendEvent,event,(window,fromConfigure),_,_) =
    {
      sendEvent        = sendEvent,
      event            = event,
      window           = window,
      fromConfigure    = fromConfigure
    };
    
    fun MakeXMapEvent (_,sendEvent,event,(window,overrideRedirect),_,_) =
    {
      sendEvent        = sendEvent,
      event            = event,
      window           = window,
      overrideRedirect = overrideRedirect
    };
    
    fun MakeXFocusChangeEvent (_,sendEvent,window,(mode,detail),_,_) =
    {
      sendEvent = sendEvent,
      window    = window,
      mode      = toNotifyMode mode,
      detail    = toNotifyDetail mode
    };
    
    fun MakeXKeymapEvent (_,sendEvent,window,keyVector,_,_) =
    {
      sendEvent = sendEvent,
      window    = window,
      keyVector = keyVector
    };
    
    fun MakeXConfigureEvent (_,sendEvent,event,(window,position,size,borderWidth,above,overrideRedirect),_,_) =
    {
      sendEvent        = sendEvent,
      event            = event,
      window           = window,
      position         = position,
      size             = size,
      borderWidth      = borderWidth,
      above            = above,
      overrideRedirect = overrideRedirect
    };
    
    fun MakeXMapRequestEvent (_,sendEvent,parent,window,_,_) =
    {
      sendEvent = sendEvent,
      parent    = parent,
      window    = window
    };
    
    fun MakeXReparentEvent (_,sendEvent,event,(window,parent,position,overrideRedirect),_,_) =
    {
      sendEvent        = sendEvent,
      event            = event,
      window           = window,
      parent           = parent,
      position         = position,
      overrideRedirect = overrideRedirect
    };
    
    fun MakeXConfigureRequestEvent (_,sendEvent,parent,(window,position,size,borderWidth,above,detail),_,_) =
    {
      sendEvent   = sendEvent,
      parent      = parent,
      window      = window,
      position    = position,
      size        = size,
      borderWidth = borderWidth,
      above       = above,
      detail      = toStackMode detail
    };
    
    fun MakeXGravityEvent (_,sendEvent,event,(window,position),_,_) =
    {
      sendEvent = sendEvent,
      event     = event,
      window    = window,
      position  = position
    };
    
    fun MakeXResizeRequestEvent (_,sendEvent,window,size,_,_) =
    {
      sendEvent = sendEvent,
      window    = window,
      size      = size
    };
    
    fun MakeXCirculateEvent (_,sendEvent,event,(window,place),_,_) =
    {
      sendEvent = sendEvent,
      event     = event,
      window    = window,
      place     = toPlacement place
    };
    
    fun MakeXCirculateRequestEvent (_,sendEvent,parent,(window,place),_,_) =
    {
      sendEvent = sendEvent,
      parent    = parent,
      window    = window,
      place     = toPlacement place
    };
    
    fun MakeXSelectionClearEvent (_,sendEvent,window,(selection,time),_,_) =
    {
      sendEvent = sendEvent,
      window    = window,
      selection = selection,
      time      = time
    }
    
    fun MakeXSelectionEvent (_,sendEvent,requestor,(selection,target,property,time),_,_) =
    {
      sendEvent = sendEvent,
      requestor = requestor,
      selection = selection,
      target    = target,
      property  = property,
      time      = time
    }
    
    fun MakeXSelectionRequestEvent (_,sendEvent,owner,(requestor,selection,target,property,time),_,_) =
    {
      sendEvent = sendEvent,
      owner     = owner,
      requestor = requestor,
      selection = selection,
      target    = target,
      property  = property,
      time      = time
    }
    
    fun MakeXColormapEvent (_,sendEvent,window,(colormap,new,installed),_,_) =
    {
      sendEvent = sendEvent,
      window    = window,
      colormap  = colormap,
      new       = new,
      installed = installed
    };
    
    fun MakeXClickEvent (_,sendEvent,window,(root,subwindow,time,pointer,rootPointer,modifiers,button,up,down),_,_) =
    {
      sendEvent   = sendEvent,
      window      = window,
      root        = root,
      subwindow   = subwindow,
      time        = time,
      pointer     = pointer,
      rootPointer = rootPointer,
      modifiers   = toModifier modifiers,
      button      = toButtonName button,
      up          = up,
      down        = down
    };
    
    fun MakeDeleteEvent (_,_,window,_,_,_) = { window = window };
    
    fun MakeMessageEvent (_,_,window,message,_,_) = { window = window, message = message };
    
    fun NextEvent display =
    let
      val (data as (etype,_,window,_,callbacks, events)) =
        xcall (XCALL_NextEvent,display);
    
      val event = case etype of
                    2   => KeyPress         (MakeXKeyEvent              (RunCall.unsafeCast data))
                  | 3   => KeyRelease       (MakeXKeyEvent              (RunCall.unsafeCast data))
                  | 4   => ButtonPress      (MakeXButtonEvent           (RunCall.unsafeCast data))
                  | 5   => ButtonRelease    (MakeXButtonEvent           (RunCall.unsafeCast data))
                  | 6   => MotionNotify     (MakeXMotionEvent           (RunCall.unsafeCast data))
                  | 7   => EnterNotify      (MakeXCrossingEvent         (RunCall.unsafeCast data))
                  | 8   => LeaveNotify      (MakeXCrossingEvent         (RunCall.unsafeCast data))
                  | 9   => FocusIn          (MakeXFocusChangeEvent      (RunCall.unsafeCast data))
                  | 10  => FocusOut         (MakeXFocusChangeEvent      (RunCall.unsafeCast data))
                  | 11  => KeymapNotify     (MakeXKeymapEvent           (RunCall.unsafeCast data))
                  | 12  => Expose           (MakeXExposeEvent           (RunCall.unsafeCast data))
                  | 13  => GraphicsExpose   (MakeXGraphicsExposeEvent   (RunCall.unsafeCast data))
                  | 14  => NoExpose         (MakeXNoExposeEvent         (RunCall.unsafeCast data))
                  | 15  => VisibilityNotify (MakeXVisibilityEvent       (RunCall.unsafeCast data))
                  | 16  => CreateNotify     (MakeXCreateWindowEvent     (RunCall.unsafeCast data))
                  | 17  => DestroyNotify    (MakeXDestroyWindowEvent    (RunCall.unsafeCast data))
                  | 18  => UnmapNotify      (MakeXUnmapEvent            (RunCall.unsafeCast data))
                  | 19  => MapNotify        (MakeXMapEvent              (RunCall.unsafeCast data))
                  | 22  => ConfigureNotify  (MakeXConfigureEvent        (RunCall.unsafeCast data))
                  | 20  => MapRequest       (MakeXMapRequestEvent       (RunCall.unsafeCast data))
                  | 21  => ReparentNotify   (MakeXReparentEvent         (RunCall.unsafeCast data))
                  | 23  => ConfigureRequest (MakeXConfigureRequestEvent (RunCall.unsafeCast data))
                  | 24  => GravityNotify    (MakeXGravityEvent          (RunCall.unsafeCast data))
                  | 25  => ResizeRequest    (MakeXResizeRequestEvent    (RunCall.unsafeCast data))
                  | 26  => CirculateNotify  (MakeXCirculateEvent        (RunCall.unsafeCast data))
                  | 27  => CirculateRequest (MakeXCirculateRequestEvent (RunCall.unsafeCast data))
                  | 29  => SelectionClear   (MakeXSelectionClearEvent   (RunCall.unsafeCast data))
                  | 30  => SelectionRequest (MakeXSelectionRequestEvent (RunCall.unsafeCast data))
                  | 31  => SelectionNotify  (MakeXSelectionEvent        (RunCall.unsafeCast data))
                  | 32  => ColormapNotify   (MakeXColormapEvent         (RunCall.unsafeCast data))
                  | 42  => ButtonClick      (MakeXClickEvent            (RunCall.unsafeCast data))
                  | 43  => DeleteRequest    (MakeDeleteEvent            (RunCall.unsafeCast data))
                  | 99  => Message          (MakeMessageEvent           (RunCall.unsafeCast data))
                  | 100 => NoEvent
                  
                  | _  => raise XWindows "Bad event type";
    in
      (event,window,callbacks,events)
    end;
    
    fun ProcessEvent (event,window,[],[]) () =
      let
        val (handler,state) = xcall (XCALL_GetState,window);
            
        val newState = handler (event,state);
      in
        if DrawableExists window
        then
          let
            val (newHandler,_) = xcall (XCALL_GetState,window);
          in
            if PolyML.pointerEq(handler, newHandler)
            then xcall (XCALL_SetState,window,nil,newState) else ()
          end
        else ()
      end
    | ProcessEvent (event,_,callbacks,events) () =
      let
        fun Call []         = ()
        |   Call ((w,f)::T) = 
            (
              Call T;
              xcall (XCALL_SetWidgetState,w,f (w,event,xcall (XCALL_GetWidgetState,w)))
            );
        fun call [] = ()
        |   call ((w,f)::T) = (call T; f (w, event))
      in
        Call callbacks;
        call events
      end;

    datatype 'a Option = None | Some of 'a;
    
    fun XWindowHandler display =
    let
      val E = Some (NextEvent display) handle XWindows _ => None;
    in
      case E of
        None   => ()
      | Some e =>
        (
          PolyML.exception_trace (ProcessEvent e)
          handle XWindows s => print("Exception XWindows \"" ^ s ^ "\" raised.\n")
          |      _          => ();
            
          XWindowHandler display
        )
    end;

  in

    fun XStartDisplayHandler display = (Thread.Thread.fork (fn () => XWindowHandler display, []); ());
    
    fun XOpenDisplay (s:string):Display =
    let
      val display = xcall (XCALL_XOpenDisplay,s);
    in
      XStartDisplayHandler display;
      display
    end; 
  
  end (* local *);
  
  val CurrentTime = 0;
  
  datatype RevertCode = RevertToNone | RevertToPointerRoot | RevertToParent;
  
  fun fromRevertCode RevertToNone        = 0
  |   fromRevertCode RevertToPointerRoot = 1
  |   fromRevertCode RevertToParent      = 2;
  
  fun toRevertCode 0 = RevertToNone
  |   toRevertCode 1 = RevertToPointerRoot
  |   toRevertCode _ = RevertToParent;
  
  fun XSetInputFocus (d:Display) (w:Drawable) (r:RevertCode) (t:int) = 
    xcall (XCALL_XSetInputFocus,d,w,fromRevertCode r,t);
  
  fun XGetInputFocus (d:Display):(Drawable * RevertCode) =
  let
    val (w,r) = xcall (XCALL_XGetInputFocus,d);
  in
    (w,toRevertCode r)
  end;
  
  fun XSetSelectionOwner (d:Display) (selection:int) (owner:Drawable) (time:int):unit =
    xcall (XCALL_XSetSelectionOwner,d,selection,owner,time);
  
  fun XGetSelectionOwner (d:Display) (selection:int):Drawable =
    xcall (XCALL_XGetSelectionOwner,d,selection);
  
  fun XConvertSelection {selection:int,target:int,property:int,requestor:Drawable,time:int}:unit =
    xcall (XCALL_XConvertSelection,selection,target,property,requestor,time);
  
  fun XSendSelectionNotify {selection:int,target:int,property:int,requestor:Drawable,time:int}:unit =
    xcall (XCALL_XSendSelectionNotify,selection,target,property,requestor,time);
  
  fun XDeleteProperty (requestor:Drawable) (property:int):unit =
    xcall (XCALL_XDeleteProperty,requestor,property);
  
  fun XInternAtom (d:Display) (name:string) (onlyIfExists:bool):int =
    xcall (XCALL_XInternAtom,d,name,onlyIfExists);
  
  fun XGetAtomName (d:Display) (atom:int):string =
    xcall (XCALL_XGetAtomName,d,atom);
  
  (* Fonts 400 *)
  
  datatype FontDirection = FontLeftToRight | FontRightToLeft;
  
  datatype XCharStruct = XCharStruct of { lbearing:   int,
                                          rbearing:   int,
                                          width:      int,
                                          ascent:     int,
                                          descent:    int,
                                          attributes: int };
  
  datatype XFontStruct = XFontStruct of { font:          Font,
                                          direction:     FontDirection,
                                          minChar:       int,
                                          maxChar:       int,
                                          minByte1:      int,
                                          maxByte1:      int,
                                          allCharsExist: bool,
                                          defaultChar:   int,
                                          minBounds:     XCharStruct,
                                          maxBounds:     XCharStruct,
                                          perChar:       XCharStruct list,
                                          ascent:        int,
                                          descent:       int };

  fun XGetFontPath (d:Display):string list =
    xcall (XCALL_XGetFontPath,d);
  
  fun XListFonts (d:Display) (s:string) (n:int):string list =
    xcall (XCALL_XListFonts,d,s,n);
  
  fun XListFontsWithInfo (d:Display) (s:string) (n:int):(string list * XFontStruct list) =
    xcall (XCALL_XListFontsWithInfo,d,s,n);

  fun XLoadFont (d:Display) (s:string):Font =
    xcall (XCALL_XLoadFont,d,s);
  
  fun XLoadQueryFont (d:Display) (s:string):XFontStruct =
    xcall (XCALL_XLoadQueryFont,d,s);
  
  fun XQueryFont (f:Font):XFontStruct =
    xcall (XCALL_XQueryFont,f);

  fun XSetFontPath (d:Display) (L:string list):unit =
    xcall (XCALL_XSetFontPath,d,L);

  (* Grabbing 450 *)
  
  (* Graphics Context 500 *)
  
  datatype GCArcMode = ArcChord | ArcPieSlice;
  
  fun fromArcMode ArcChord    = 0
  |   fromArcMode ArcPieSlice = 1;

  datatype GCOrder = Unsorted | YSorted | YXSorted | YXBanded;

  fun fromOrder Unsorted = 0
  |   fromOrder YSorted  = 1
  |   fromOrder YXSorted = 2
  |   fromOrder YXBanded = 3;
  
  datatype GCFillRule = EvenOddRule | WindingRule;

  fun fromFillRule EvenOddRule = 0
  |   fromFillRule WindingRule = 1;
  
  datatype GCFillStyle = FillSolid | FillTiled | FillStippled | FillOpaqueStippled;

  fun fromFillStyle FillSolid          = 0
  |   fromFillStyle FillTiled          = 1
  |   fromFillStyle FillStippled       = 2
  |   fromFillStyle FillOpaqueStippled = 3;
  
  datatype GCFunction = GXclear        | GXand        | GXandReverse | GXcopy
                      | GXandInverted  | GXnoop       | GXxor        | GXor
                      | GXnor          | GXequiv      | GXinvert     | GXorReverse
                      | GXcopyInverted | GXorInverted | GXnand       | GXset;

  fun fromFunction GXclear        = 0
  |   fromFunction GXand          = 1
  |   fromFunction GXandReverse   = 2
  |   fromFunction GXcopy         = 3
  |   fromFunction GXandInverted  = 4
  |   fromFunction GXnoop         = 5
  |   fromFunction GXxor          = 6
  |   fromFunction GXor           = 7
  |   fromFunction GXnor          = 8
  |   fromFunction GXequiv        = 9
  |   fromFunction GXinvert       = 10
  |   fromFunction GXorReverse    = 11
  |   fromFunction GXcopyInverted = 12
  |   fromFunction GXorInverted   = 13
  |   fromFunction GXnand         = 14
  |   fromFunction GXset          = 15;
  
  datatype GCLineStyle = LineSolid | LineOnOffDash | LineDoubleDash;

  fun fromLineStyle LineSolid      = 0
  |   fromLineStyle LineOnOffDash  = 1
  |   fromLineStyle LineDoubleDash = 2;

  datatype GCCapStyle = CapNotLast | CapButt | CapRound | CapProjecting;
  
  fun fromCapStyle CapNotLast    = 0
  |   fromCapStyle CapButt       = 1
  |   fromCapStyle CapRound      = 2
  |   fromCapStyle CapProjecting = 3;

  datatype GCJoinStyle = JoinMiter | JoinRound | JoinBevel;

  fun fromJoinStyle JoinMiter = 0
  |   fromJoinStyle JoinRound = 1
  |   fromJoinStyle JoinBevel = 2;
  
  datatype GCSubwindowMode = ClipByChildren | IncludeInferiors;

  fun fromSubwindowMode ClipByChildren   = 0
  |   fromSubwindowMode IncludeInferiors = 1;
  
  fun DefaultGC (d:Display) = xcall (XCALL_DefaultGC,d) : GC;

  datatype XGCValue = GCFunction          of GCFunction
                    | GCPlaneMask         of int
                    | GCForeground        of int
                    | GCBackground        of int
                    | GCLineWidth         of int
                    | GCLineStyle         of GCLineStyle
                    | GCCapStyle          of GCCapStyle
                    | GCJoinStyle         of GCJoinStyle
                    | GCFillStyle         of GCFillStyle
                    | GCFillRule          of GCFillRule
                    | GCTile              of Drawable
                    | GCStipple           of Drawable
                    | GCTSOrigin          of XPoint
                    | GCFont              of Font
                    | GCSubwindowMode     of GCSubwindowMode
                    | GCGraphicsExposures of bool
                    | GCClipOrigin        of XPoint
                    | GCClipMask          of Drawable
                    | GCDashOffset        of int
                    | GCDashList          of int
                    | GCArcMode           of GCArcMode;

  fun UpdateGC gc (GCFunction f)                =  xcall (XCALL_UpdateGC,gc,0 ,fromFunction f)
  |   UpdateGC gc (GCPlaneMask mask)            =  xcall (XCALL_UpdateGC,gc,1 ,mask)
  |   UpdateGC gc (GCForeground fg)             =  xcall (XCALL_UpdateGC,gc,2 ,fg)
  |   UpdateGC gc (GCBackground bg)             =  xcall (XCALL_UpdateGC,gc,3 ,bg)
  |   UpdateGC gc (GCLineWidth w)               =  xcall (XCALL_UpdateGC,gc,4 ,w)
  |   UpdateGC gc (GCLineStyle ls)              =  xcall (XCALL_UpdateGC,gc,5 ,fromLineStyle ls)
  |   UpdateGC gc (GCCapStyle cs)               =  xcall (XCALL_UpdateGC,gc,6 ,fromCapStyle  cs)
  |   UpdateGC gc (GCJoinStyle js)              =  xcall (XCALL_UpdateGC,gc,7 ,fromJoinStyle js)
  |   UpdateGC gc (GCFillStyle fs)              =  xcall (XCALL_UpdateGC,gc,8 ,fromFillStyle fs)
  |   UpdateGC gc (GCFillRule rule)             =  xcall (XCALL_UpdateGC,gc,9 ,fromFillRule  rule)
  |   UpdateGC gc (GCTile tile)                 =  xcall (XCALL_UpdateGC,gc,10,tile)
  |   UpdateGC gc (GCStipple stipple)           =  xcall (XCALL_UpdateGC,gc,11,stipple)
  |   UpdateGC gc (GCTSOrigin (XPoint {x,y}))   = (xcall (XCALL_UpdateGC,gc,12,x); xcall (XCALL_UpdateGC,gc,13,y))
  |   UpdateGC gc (GCFont font)                 =  xcall (XCALL_UpdateGC,gc,14,font)
  |   UpdateGC gc (GCSubwindowMode mode)        =  xcall (XCALL_UpdateGC,gc,15,fromSubwindowMode mode)
  |   UpdateGC gc (GCGraphicsExposures ex)      =  xcall (XCALL_UpdateGC,gc,16,ex)
  |   UpdateGC gc (GCClipOrigin (XPoint {x,y})) = (xcall (XCALL_UpdateGC,gc,17,x); xcall (XCALL_UpdateGC,gc,18,y))
  |   UpdateGC gc (GCClipMask mask)             =  xcall (XCALL_UpdateGC,gc,19,mask)
  |   UpdateGC gc (GCDashOffset offset)         =  xcall (XCALL_UpdateGC,gc,20,offset)
  |   UpdateGC gc (GCDashList dashes)           =  xcall (XCALL_UpdateGC,gc,21,dashes)
  |   UpdateGC gc (GCArcMode mode)              =  xcall (XCALL_UpdateGC,gc,22,fromArcMode mode);
  
  fun XChangeGC gc []     = ()
  |   XChangeGC gc (H::T) = (UpdateGC gc H; XChangeGC gc T);
  
  fun XCreateGC (d:Drawable) (values:XGCValue list):GC =
  let
    val gc = xcall (XCALL_XCreateGC,d);
  in
    XChangeGC gc values;
    gc
  end;

  fun XSetFunction          gc f       = UpdateGC gc (GCFunction f);
  fun XSetPlaneMask         gc mask    = UpdateGC gc (GCPlaneMask mask);
  fun XSetForeground        gc fg      = UpdateGC gc (GCForeground fg);
  fun XSetBackground        gc bg      = UpdateGC gc (GCBackground bg);
  fun XSetFillStyle         gc fs      = UpdateGC gc (GCFillStyle fs);
  fun XSetFillRule          gc rule    = UpdateGC gc (GCFillRule rule);
  fun XSetTile              gc tile    = UpdateGC gc (GCTile tile)
  fun XSetStipple           gc stipple = UpdateGC gc (GCStipple stipple);
  fun XSetTSOrigin          gc point   = UpdateGC gc (GCTSOrigin point);
  fun XSetFont              gc font    = UpdateGC gc (GCFont font);
  fun XSetSubwindowMode     gc mode    = UpdateGC gc (GCSubwindowMode mode);
  fun XSetGraphicsExposures gc ex      = UpdateGC gc (GCGraphicsExposures ex);
  fun XSetClipOrigin        gc point   = UpdateGC gc (GCClipOrigin point);
  fun XSetClipMask          gc mask    = UpdateGC gc (GCClipMask mask);
  fun XSetArcMode           gc mode    = UpdateGC gc (GCArcMode mode);

  fun XSetLineAttributes gc w  ls cs js   = XChangeGC gc [GCLineWidth  w, GCLineStyle  ls,GCCapStyle cs,GCJoinStyle js];
  fun XSetState          gc fg bg f  mask = XChangeGC gc [GCForeground fg,GCBackground bg,GCFunction f, GCPlaneMask mask];

  fun XSetClipRectangles (gc:GC) (p:XPoint) (L:XRectangle list) (order:GCOrder):unit =
    xcall (XCALL_XSetClipRectangles,gc,p,L,fromOrder order);
  
  fun XSetDashes (gc:GC) (offset:int) (dashes:int list) =
    xcall (XCALL_XSetDashes,gc,offset,dashes);
  
  (* Images 550 *)
  
  datatype ImageFormat = XYBitmap | XYPixmap | ZPixmap;
  
  datatype ImageOrder = LSBFirst | MSBFirst;

  abstype ImageData = ImageData of string with val Data = ImageData end;
  
  datatype XImage = XImage of { data:            ImageData,
                                size:            XRectangle,
                                depth:           int,
                                format:          ImageFormat,
                                xoffset:         int,
                                bitmapPad:       int,
                                byteOrder:       ImageOrder,
                                bitmapUnit:      int,
                                bitsPerPixel:    int,
                                bytesPerLine:    int,
                                visualRedMask:   int,
                                bitmapBitOrder:  ImageOrder,
                                visualBlueMask:  int,
                                visualGreenMask: int };
                                
  (* XImages need to be mutable for XAddPixel, XGetSubImage and XPutPixel to work properly *)
  
  fun XAddPixel (d:Display) (i:XImage) (a:int):unit =
    xcall (XCALL_XAddPixel,d,i,a);

  fun XCreateImage v depth format xoffset data size bitmapPad bytesPerLine =
  (
    XImage { data            = Data data,
             size            = size,
             depth           = depth,
             format          = format,
             xoffset         = xoffset,
             bitmapPad       = bitmapPad,
             byteOrder       = LSBFirst,
             bitmapUnit      = 8,
             bitsPerPixel    = 1,
             bytesPerLine    = bytesPerLine,
             visualRedMask   = VisualRedMask v,
             bitmapBitOrder  = LSBFirst,
             visualBlueMask  = VisualBlueMask v,
             visualGreenMask = VisualGreenMask v }
  );

  fun XGetImage (d:Drawable) (r:XRectangle) (mask:int) (f:ImageFormat) =
    XImage (xcall (XCALL_XGetImage,d,r,mask,f));

  fun XGetPixel (d:Display) (i:XImage) (point:XPoint):int =
    xcall (XCALL_XGetPixel,d,i,point);

  fun XGetSubImage (d:Drawable) (r:XRectangle) (mask:int) (f:ImageFormat) (i:XImage) (point:XPoint):unit =
    xcall (XCALL_XGetSubImage,d,r,mask,f,i,point);
  
  fun XPutImage (d:Drawable) (gc:GC) (i:XImage) (p:XPoint) (r:XRectangle):unit =
    xcall (XCALL_XPutImage,d,gc,i,p,r);

  fun XPutPixel (d:Display) (i:XImage) (point:XPoint) (pixel:int):unit =
    xcall (XCALL_XPutPixel,d,i,point,pixel);

  fun XSubImage (d:Display) (i:XImage) (r:XRectangle) =
    XImage (xcall (XCALL_XSubImage,d,i,r));
      
  fun BitmapBitOrder (d:Display) = xcall (XCALL_BitmapBitOrder,d) : ImageOrder;
  fun BitmapPad (d:Display)      = xcall (XCALL_BitmapPad,d) : int;
  fun BitmapUnit (d:Display)     = xcall (XCALL_BitmapUnit,d) : int;
  fun ByteOrder (d:Display)      = xcall (XCALL_ByteOrder,d) : ImageOrder;
  
  fun ImageByteOrder (XImage {byteOrder,...}) = byteOrder;
  fun ImageSize      (XImage {size,...})      = size;
  fun ImageDepth     (XImage {depth,...})     = depth;

  (* Keyboard 600 *)
  
  (* keysym values are integers, and are defined in other signatures *)

  fun XLookupString (d:Drawable) (keycode:int) (modifiers:Modifier list):(string * int) =
     xcall (XCALL_XLookupString,d,keycode,fromModifier modifiers);
  
  fun XQueryKeymap (d:Display):bool list = xcall (XCALL_XQueryKeymap,d);

  fun IsCursorKey       (k:int):bool = xcall (XCALL_IsCursorKey,k);
  fun IsFunctionKey     (k:int):bool = xcall (XCALL_IsFunctionKey,k);
  fun IsKeypadKey       (k:int):bool = xcall (XCALL_IsKeypadKey,k);
  fun IsMiscFunctionKey (k:int):bool = xcall (XCALL_IsMiscFunctionKey,k);
  fun IsModifierKey     (k:int):bool = xcall (XCALL_IsModifierKey,k);
  fun IsPFKey           (k:int):bool = xcall (XCALL_IsPFKey,k);
  
  (* Output Buffer 650 *)
  
  fun XFlush (d:Display):unit = xcall (XCALL_XFlush,d);
  
  fun XSync (d:Display) (b:bool):unit = xcall (XCALL_XSync,d,b);

  (* Pointers 700 *)
  
  fun XQueryPointer (d:Drawable) =
  let
    val (s:bool,root:Drawable,child:Drawable,r:XPoint,w:XPoint,m) =
      xcall (XCALL_XQueryPointer,d);
  in
    (s,root,child,r,w,toModifier m)
  end;

  (* Regions 750*)
  
  (* Save Set 800 *)
  
  (* Screen Saver 850 *)
  
  datatype SaveMode = ScreenSaverReset | ScreenSaverActive;
  
  fun fromSaveMode ScreenSaverReset  = 0
  |   fromSaveMode ScreenSaverActive = 1;
  
  datatype Blanking = DontPreferBlanking | PreferBlanking | DefaultBlanking;
  
  fun toBlanking 0 = DontPreferBlanking
  |   toBlanking 1 = PreferBlanking
  |   toBlanking _ = DefaultBlanking;
  
  fun fromBlanking DontPreferBlanking = 0
  |   fromBlanking PreferBlanking     = 1
  |   fromBlanking DefaultBlanking    = 2;
  
  datatype Exposures = DontAllowExposures | AllowExposures | DefaultExposures;
  
  fun toExposures 0 = DontAllowExposures
  |   toExposures 1 = AllowExposures
  |   toExposures _ = DefaultExposures;
  
  fun fromExposures DontAllowExposures = 0
  |   fromExposures AllowExposures     = 1
  |   fromExposures DefaultExposures   = 2;
  
  fun XActivateScreenSaver (d:Display):unit =
    xcall (XCALL_XActivateScreenSaver,d);

  fun XForceScreenSaver (d:Display) (s:SaveMode):unit =
    xcall (XCALL_XForceScreenSaver,d,fromSaveMode s);
  
  fun XGetScreenSaver (d:Display) =
  let
    val (timeout,interval,blanking,exposures) =
      xcall (XCALL_XGetScreenSaver,d);
  in
    (timeout:int,interval:int,toBlanking blanking,toExposures exposures)
  end;

  fun XResetScreenSaver (d:Display):unit =
    xcall (XCALL_XResetScreenSaver,d);

  fun XSetScreenSaver (d:Display)
                      (timeout:int)
                      (interval:int)
                      (blanking:Blanking)
                      (exposures:Exposures) =
    xcall (XCALL_XSetScreenSaver,d,timeout,interval,fromBlanking blanking,fromExposures exposures);

  (* Standard Geometry 900 *)
  
  fun XTranslateCoordinates (src:Drawable) (dst:Drawable) (p:XPoint):(XPoint * Drawable) =
    xcall (XCALL_XTranslateCoordinates,src,dst,p);
  
  (* Text 950 *)
  
  fun XTextExtents (fs:XFontStruct) (s:string):(FontDirection * int * int * XCharStruct) =
    xcall (XCALL_XTextExtents,fs,s);

  fun XTextExtents16 (fs:XFontStruct) (L:int list):(FontDirection * int * int * XCharStruct) =
    xcall (XCALL_XTextExtents16,fs,L);

  fun XTextWidth (fs:XFontStruct) (s:string):int =
    xcall (XCALL_XTextWidth,fs,s);

  fun XTextWidth16 (fs:XFontStruct) (L:int list):int =
    xcall (XCALL_XTextWidth16,fs,L);

  (* Tiles, Pixmaps, Stipples and Bitmaps 1000 *)
  
  datatype BitmapStatus = BitmapOpenFailed
                        | BitmapFileInvalid
                        | BitmapNoMemory
                        | BitmapSuccess of Drawable * XRectangle * XPoint;
  
  fun XCreateBitmapFromData (d:Drawable) (s:string) (r:XRectangle):Drawable =
    xcall (XCALL_XCreateBitmapFromData,d,s,r);

  fun XCreatePixmap (d:Drawable) (r:XRectangle) (depth:int):Drawable =
    xcall (XCALL_XCreatePixmap,d,r,depth);

  fun XCreatePixmapFromBitmapData (d:Drawable)
                                  (s:string)
                                  (r:XRectangle)
                                  (fg:int)
                                  (bg:int)
                                  (depth:int):Drawable =
    xcall (XCALL_XCreatePixmapFromBitmapData,d,s,r,fg,bg,depth);

  fun XQueryBestStipple (d:Drawable) (r:XRectangle):XRectangle =
    xcall (XCALL_XQueryBestStipple,d,r);
    
  fun XQueryBestTile    (d:Drawable) (r:XRectangle):XRectangle =
    xcall (XCALL_XQueryBestTile,d,r);

  datatype ShapeClass = CursorShape | TileShape | StippleShape;
  
  fun XQueryBestSize CursorShape  = XQueryBestCursor
  |   XQueryBestSize TileShape    = XQueryBestTile
  |   XQueryBestSize StippleShape = XQueryBestStipple;
                                            
  fun XReadBitmapFile (d:Drawable) (s:string) =
  let
    val (status,bitmap,size,hotspot) = xcall (XCALL_XReadBitmapFile,d,s);
  in
    case status of
      0 => BitmapSuccess (bitmap,size,hotspot)
    | 1 => BitmapOpenFailed
    | 2 => BitmapFileInvalid
    | _ => BitmapNoMemory
  end;

  fun XWriteBitmapFile (s:string) (bitmap:Drawable) (size:XRectangle) (hotspot:XPoint) =
  let
    val status = xcall (XCALL_XWriteBitmapFile,s,bitmap,size,hotspot);
  in
    case status of
      0 => BitmapSuccess (bitmap,size,hotspot)
    | 1 => BitmapOpenFailed
    | 2 => BitmapFileInvalid
    | _ => BitmapNoMemory
  end;

  (* User Preferences 1050 *)
  
  fun XAutoRepeatOff (d:Display):unit = xcall (XCALL_XAutoRepeatOff,d);
  fun XAutoRepeatOn  (d:Display):unit = xcall (XCALL_XAutoRepeatOn,d);

  fun XBell (d:Display) (n:int):unit = xcall (XCALL_XBell,d,n);

  fun XGetDefault (d:Display) (program:string) (option:string):string =
     xcall (XCALL_XGetDefault,d,program,option);

  (* Window Attributes 1100 *)
  
  datatype Gravity =
    ForgetGravity    | NorthWestGravity | NorthGravity
  | NorthEastGravity | WestGravity      | CenterGravity
  | EastGravity      | SouthWestGravity | SouthGravity
  | SouthEastGravity | StaticGravity;
  
  val UnmapGravity = ForgetGravity;

  fun toGravity 0 = ForgetGravity
  |   toGravity 1 = NorthWestGravity
  |   toGravity 2 = NorthGravity
  |   toGravity 3 = NorthEastGravity
  |   toGravity 4 = WestGravity
  |   toGravity 5 = CenterGravity
  |   toGravity 6 = EastGravity
  |   toGravity 7 = SouthWestGravity
  |   toGravity 8 = SouthGravity
  |   toGravity 9 = SouthEastGravity
  |   toGravity _ = StaticGravity;
  
  fun fromGravity ForgetGravity    = 0
  |   fromGravity NorthWestGravity = 1
  |   fromGravity NorthGravity     = 2
  |   fromGravity NorthEastGravity = 3
  |   fromGravity WestGravity      = 4
  |   fromGravity CenterGravity    = 5
  |   fromGravity EastGravity      = 6
  |   fromGravity SouthWestGravity = 7
  |   fromGravity SouthGravity     = 8
  |   fromGravity SouthEastGravity = 9
  |   fromGravity StaticGravity    = 10;

  datatype XSetWindowAttributes = CWBackPixmap       of Drawable
                                | CWBackPixel        of int
                                | CWBorderPixmap     of Drawable
                                | CWBorderPixel      of int
                                | CWBitGravity       of Gravity
                                | CWWinGravity       of Gravity
                                | CWBackingStore     of BackingStore
                                | CWBackingPlanes    of int
                                | CWBackingPixel     of int
                                | CWOverrideRedirect of bool
                                | CWSaveUnder        of bool
                                | CWEventMask        of EventMask list
                                | CWDontPropagate    of EventMask list
                                | CWColormap         of Colormap
                                | CWCursor           of Cursor;
  
  datatype WindowClass = CopyFromParentClass | InputOutputClass | InputOnlyClass;

  fun toWindowClass 0 = CopyFromParentClass
  |   toWindowClass 1 = InputOutputClass
  |   toWindowClass _ = InputOnlyClass;
  
  fun fromWindowClass CopyFromParentClass = 0
  |   fromWindowClass InputOutputClass    = 1
  |   fromWindowClass InputOnlyClass      = 2;
  
  datatype MapState = IsUnmapped | IsUnviewable | IsViewable;
  
  fun toMapState 0 = IsUnmapped
  |   toMapState 1 = IsUnviewable
  |   toMapState _ = IsViewable;
  
  datatype XWindowAttributes = XWindowAttributes of
                               {
                                 position:           XPoint,
                                 size:               XRectangle,
                                 borderWidth:        int,
                                 depth:              int,
                                 visual:             Visual,
                                 root:               Drawable,
                                 class:              WindowClass,
                                 bitGravity:         Gravity,
                                 winGravity:         Gravity,
                                 backingStore:       BackingStore,
                                 backingPlanes:      int,
                                 backingPixel:       int,
                                 saveUnder:          bool,
                                 colormap:           Colormap,
                                 mapInstalled:       bool,
                                 mapState:           MapState,
                                 allEventMasks:      EventMask list,
                                 yourEventMask:      EventMask list,
                                 doNotPropagateMask: EventMask list,
                                 overrideRedirect:   bool
                               };
  
  fun ChangeWindow d (CWBackPixmap pixmap)   = xcall (XCALL_ChangeWindow,d,0 ,pixmap)
  |   ChangeWindow d (CWBackPixel pixel)     = xcall (XCALL_ChangeWindow,d,1 ,pixel)
  |   ChangeWindow d (CWBorderPixmap pixmap) = xcall (XCALL_ChangeWindow,d,2 ,pixmap)
  |   ChangeWindow d (CWBorderPixel pixel)   = xcall (XCALL_ChangeWindow,d,3 ,pixel)
  |   ChangeWindow d (CWBitGravity gravity)  = xcall (XCALL_ChangeWindow,d,4 ,fromGravity gravity)
  |   ChangeWindow d (CWWinGravity gravity)  = xcall (XCALL_ChangeWindow,d,5 ,fromGravity gravity)
  |   ChangeWindow d (CWBackingStore  bs)    = xcall (XCALL_ChangeWindow,d,6 ,fromBackingStore bs)
  |   ChangeWindow d (CWBackingPlanes mask)  = xcall (XCALL_ChangeWindow,d,7 ,mask)
  |   ChangeWindow d (CWBackingPixel pixel)  = xcall (XCALL_ChangeWindow,d,8 ,pixel)
  |   ChangeWindow d (CWOverrideRedirect b)  = xcall (XCALL_ChangeWindow,d,9 ,b)
  |   ChangeWindow d (CWSaveUnder b)         = xcall (XCALL_ChangeWindow,d,10,b)
  |   ChangeWindow d (CWEventMask mask)      = xcall (XCALL_ChangeWindow,d,11,fromEventMask mask)
  |   ChangeWindow d (CWDontPropagate mask)  = xcall (XCALL_ChangeWindow,d,12,fromEventMask mask)
  |   ChangeWindow d (CWColormap cmap)       = xcall (XCALL_ChangeWindow,d,13,cmap)
  |   ChangeWindow d (CWCursor cursor)       = xcall (XCALL_ChangeWindow,d,14,cursor);

  fun XChangeWindowAttributes d []     = ()
  |   XChangeWindowAttributes d (H::T) = (ChangeWindow d H; XChangeWindowAttributes d T);
  
  fun XGetGeometry (d:Drawable):(Drawable * XPoint * XRectangle * int * int) =
    xcall (XCALL_XGetGeometry,d);

  fun XGetWindowAttributes (d:Drawable) =
  let
    val (position,size,borderWidth,depth,visual,root,class,
         bitGravity,winGravity,backingStore,backingPlanes,
         backingPixel,saveUnder,colormap,mapInstalled,mapState,
         allEventMasks,yourEventMask,doNotPropagateMask,overrideRedirect) =
      xcall (XCALL_XGetWindowAttributes,d);
  in
    XWindowAttributes { position           = position,
                        size               = size,
                        borderWidth        = borderWidth,
                        depth              = depth,
                        visual             = visual,
                        root               = root,
                        class              = toWindowClass class,
                        bitGravity         = toGravity bitGravity,
                        winGravity         = toGravity winGravity,
                        backingStore       = toBackingStore backingStore,
                        backingPlanes      = backingPlanes,
                        backingPixel       = backingPixel,
                        saveUnder          = saveUnder,
                        colormap           = colormap,
                        mapInstalled       = mapInstalled,
                        mapState           = toMapState  mapState,
                        allEventMasks      = toEventMask allEventMasks,
                        yourEventMask      = toEventMask yourEventMask,
                        doNotPropagateMask = toEventMask doNotPropagateMask,
                        overrideRedirect   = overrideRedirect }
  end;

  fun XSetWindowBackground       d bg     = ChangeWindow d (CWBackPixel    bg);
  fun XSetWindowBackgroundPixmap d pixmap = ChangeWindow d (CWBackPixmap   pixmap);
  fun XSetWindowBorder           d border = ChangeWindow d (CWBorderPixel  border);
  fun XSetWindowBorderPixmap     d pixmap = ChangeWindow d (CWBorderPixmap pixmap);
  fun XSetWindowColormap         d cmap   = ChangeWindow d (CWColormap     cmap);

  fun XSetWindowBorderWidth (d:Drawable) (width:int):unit =
    xcall (XCALL_XSetWindowBorderWidth,d,width);
  
  (* Window Configuration 1150 *)
  
  val NoDrawable           = xcall (XCALL_NoDrawable,0) : Drawable;
  val ParentRelative       = xcall (XCALL_NoDrawable,1) : Drawable;
  val NoCursor             = xcall (XCALL_NoCursor,0) : Cursor;
  val NoFont               = xcall (XCALL_NoFont,0) : Font;
  val NoColormap           = xcall (XCALL_NoColormap,0) : Colormap;
  val NoVisual             = xcall (XCALL_NoVisual,0) : Visual;

  val CopyFromParentDrawable = NoDrawable;
  val CopyFromParentVisual   = NoVisual;
  val PointerWindow          = NoDrawable;
  val InputFocus             = ParentRelative;
  val PointerRoot            = ParentRelative;

  val NoSymbol = 0;
  val AnyKey   = 0;

  datatype CirculateDirection = RaiseLowest | LowerHighest;
  
  fun fromCirculateDirection RaiseLowest  = 0
  |   fromCirculateDirection LowerHighest = 1;
  
  datatype XWindowChanges = CWPosition    of XPoint
                          | CWSize        of XRectangle
                          | CWBorderWidth of int
                          | CWStackMode   of StackMode
                          | CWSibling     of Drawable;
  
  fun XCirculateSubwindows (d:Drawable) (dir:CirculateDirection):unit =
    xcall (XCALL_XCirculateSubwindows,d,fromCirculateDirection dir);
  
  fun XCirculateSubwindowsDown d = XCirculateSubwindows d LowerHighest;
  fun XCirculateSubwindowsUp   d = XCirculateSubwindows d RaiseLowest;

  fun XConfigureWindow (win:Drawable) (L:XWindowChanges list):unit =
  let
    fun AddIn m n = if (m And n) = 0 then (m Or n) else raise XWindows "XConfigureWindow parameters";
    
    fun Trans []                     acc           = acc
    |   Trans ((CWPosition    P)::T) (_,S,w,d,s,m) = Trans T (P,S,w,d,s,AddIn m 3)
    |   Trans ((CWSize        S)::T) (P,_,w,d,s,m) = Trans T (P,S,w,d,s,AddIn m 12)
    |   Trans ((CWBorderWidth w)::T) (P,S,_,d,s,m) = Trans T (P,S,w,d,s,AddIn m 16)
    |   Trans ((CWSibling     d)::T) (P,S,w,_,s,m) = Trans T (P,S,w,d,s,AddIn m 32)
    |   Trans ((CWStackMode   s)::T) (P,S,w,d,_,m) = Trans T (P,S,w,d,fromStackMode s,AddIn m 64);
  in
    xcall (XCALL_XConfigureWindow,win,Trans L (origin,a1,0,NoDrawable,0,0))
  end;

  fun XLowerWindow (d:Drawable):unit = xcall (XCALL_XLowerWindow,d);

  fun XMapRaised (d:Drawable):unit = xcall (XCALL_XMapRaised,d);

  fun XMapSubwindows (d:Drawable):unit = xcall (XCALL_XMapSubwindows,d);

  fun XMapWindow (d:Drawable):unit = xcall (XCALL_XMapWindow,d);

  fun XMoveResizeWindow (d:Drawable) (p:XPoint) (r:XRectangle):unit =
    xcall (XCALL_XMoveResizeWindow,d,p,r);

  fun XMoveWindow (d:Drawable) (p:XPoint):unit = xcall (XCALL_XMoveWindow,d,p);

  fun XQueryTree (d:Drawable):(Drawable * Drawable * Drawable list) =
    xcall (XCALL_XQueryTree,d);

  fun XRaiseWindow (d:Drawable):unit = xcall (XCALL_XRaiseWindow,d);

  fun XReparentWindow (win:Drawable) (parent:Drawable) (p:XPoint):unit =
    xcall (XCALL_XReparentWindow,win,parent,p);

  fun XResizeWindow (d:Drawable) (r:XRectangle):unit =
    xcall (XCALL_XResizeWindow,d,r);

  fun XRestackWindows (L:Drawable list):unit =
    xcall (XCALL_XRestackWindows,L);

  fun XUnmapSubwindows (d:Drawable):unit =
    xcall (XCALL_XUnmapSubwindows,d);

  fun XUnmapWindow (d:Drawable):unit =
    xcall (XCALL_XUnmapWindow,d);
  
  (* Window Existence 1200 *)
  
  fun RootWindow (d:Display) = xcall (XCALL_RootWindow,d) : Drawable;

  fun DestroyXObject x = xcall (XCALL_DestroyXObject,x):unit;

  val XDestroyWindow = DestroyXObject : Drawable -> unit;
  val XFreeGC        = DestroyXObject : GC       -> unit;
  val XFreePixmap    = DestroyXObject : Drawable -> unit;
  val XFreeCursor    = DestroyXObject : Cursor   -> unit;
  val XUnloadFont    = DestroyXObject : Font     -> unit;
  val XFreeColormap  = DestroyXObject : Colormap -> unit;

  fun XFreeFont (XFontStruct {font,...}) = XUnloadFont font;

  fun XDestroySubwindows (d:Drawable):unit = xcall (XCALL_XDestroySubwindows,d);

  fun XCreateSimpleWindow (parent:Drawable) (position:XPoint) (size:XRectangle) (borderWidth:int) (border:int) (background:int): Drawable =
  (
    xcall (XCALL_XCreateSimpleWindow,parent,position,size,borderWidth,border,background,NullHandler,42)
  );

  fun XCreateWindow (parent:Drawable)
                    (position:XPoint)
                    (size:XRectangle)
                    (borderWidth:int)
                    (depth:int)
                    (class:WindowClass)
                    (visual:Visual)
                    (L:XSetWindowAttributes list): Drawable =
  let
    val w = xcall (XCALL_XCreateWindow,parent,position,size,borderWidth,depth,fromWindowClass class,visual,NullHandler,42);
  in
    XChangeWindowAttributes w L;
    w
  end;

  (* Window Manager 1250 *)
  
  datatype XWMStateHint = DontCareState | NormalState | ZoomState | IconicState | InactiveState;
  
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
    
  datatype XWMHint = InputHint        of bool
                   | StateHint        of XWMStateHint
                   | IconPixmapHint   of Drawable
                   | IconWindowHint   of Drawable
                   | IconPositionHint of XPoint
                   | IconMaskHint     of Drawable;

  datatype XWMSizeHint = PPosition   of XPoint
                       | PSize       of XRectangle
                       | PMinSize    of XRectangle
                       | PMaxSize    of XRectangle
                       | PResizeInc  of XRectangle
                       | PAspect     of XPoint * XPoint
                       | PBaseSize   of XRectangle
                       | PWinGravity of Gravity;
  
  datatype XStandardColormap = XStandardColormap of { colormap:  Colormap,
                                                      redMax:    int,
                                                      redMult:   int,
                                                      greenMax:  int,
                                                      greenMult: int,
                                                      blueMax:   int,
                                                      blueMult:  int,
                                                      basePixel: int,
                                                      visual:    Visual };

  datatype PropertyValue = PropertyArc         of XArc list
                         | PropertyAtom        of int list
                         | PropertyBitmap      of Drawable list
                         | PropertyColormap    of Colormap list
                         | PropertyCursor      of Cursor list
                         | PropertyDrawable    of Drawable list
                         | PropertyFont        of Font list
                         | PropertyInteger     of int list
                         | PropertyPixmap      of Drawable list
                         | PropertyPoint       of XPoint list
                         | PropertyRectangle   of XRectangle list
                         | PropertyRGBColormap of XStandardColormap list
                         | PropertyString      of string
                         | PropertyVisual      of Visual list
                         | PropertyWindow      of Drawable list
                         | PropertyWMHints     of XWMHint list
                         | PropertyWMSizeHints of XWMSizeHint list 
                         | PropertyWMIconSizes of (XRectangle * XRectangle * XRectangle) list;
  
  fun fromXWMHints L =
  let
    fun AddIn m n = if (m And n) = 0 then (m Or n) else raise XWindows "XWMHint parameters";
    
    fun Trans []                        (i,s,p,w,P,m,f) = (i,fromXWMStateHint s,p,w,P,m,f)
    |   Trans ((InputHint        i)::T) (_,s,p,w,P,m,f) = Trans T (i,s,p,w,P,m,AddIn f 1)
    |   Trans ((StateHint        s)::T) (i,_,p,w,P,m,f) = Trans T (i,s,p,w,P,m,AddIn f 2)
    |   Trans ((IconPixmapHint   p)::T) (i,s,_,w,P,m,f) = Trans T (i,s,p,w,P,m,AddIn f 4)
    |   Trans ((IconWindowHint   w)::T) (i,s,p,_,P,m,f) = Trans T (i,s,p,w,P,m,AddIn f 8)
    |   Trans ((IconPositionHint P)::T) (i,s,p,w,_,m,f) = Trans T (i,s,p,w,P,m,AddIn f 16)
    |   Trans ((IconMaskHint     m)::T) (i,s,p,w,P,_,f) = Trans T (i,s,p,w,P,m,AddIn f 32);
  in
    Trans L (false,DontCareState,NoDrawable,NoDrawable,origin,NoDrawable,0)
  end;
  
  fun fromXWMSizeHints L =
  let
    fun AddIn m n = if (m And n) = 0 then (m Or n) else raise XWindows "XWMSizeHint parameters";
    
    fun Trans []                     (p,s,min,max,inc,a,bs,wg,f) = (p,s,min,max,inc,a,bs,fromGravity wg,f)
    |   Trans ((PPosition   p)  ::T) (_,s,min,max,inc,a,bs,wg,f) = Trans T (p,s,min,max,inc,a,bs,wg,AddIn f 4)
    |   Trans ((PSize       s)  ::T) (p,_,min,max,inc,a,bs,wg,f) = Trans T (p,s,min,max,inc,a,bs,wg,AddIn f 8)
    |   Trans ((PMinSize    min)::T) (p,s,_  ,max,inc,a,bs,wg,f) = Trans T (p,s,min,max,inc,a,bs,wg,AddIn f 16)
    |   Trans ((PMaxSize    max)::T) (p,s,min,_  ,inc,a,bs,wg,f) = Trans T (p,s,min,max,inc,a,bs,wg,AddIn f 32)
    |   Trans ((PResizeInc  inc)::T) (p,s,min,max,_  ,a,bs,wg,f) = Trans T (p,s,min,max,inc,a,bs,wg,AddIn f 64)
    |   Trans ((PAspect     a)  ::T) (p,s,min,max,inc,_,bs,wg,f) = Trans T (p,s,min,max,inc,a,bs,wg,AddIn f 128)
    |   Trans ((PBaseSize   bs) ::T) (p,s,min,max,inc,a,_ ,wg,f) = Trans T (p,s,min,max,inc,a,bs,wg,AddIn f 256)
    |   Trans ((PWinGravity wg) ::T) (p,s,min,max,inc,a,bs,_ ,f) = Trans T (p,s,min,max,inc,a,bs,wg,AddIn f 512);
  in
    Trans L (origin,a1,a1,a1,a1,(origin,origin),a1,ForgetGravity,0)
  end;
  
  fun fromXStandardColormaps L =
  let
    fun Trans (XStandardColormap {colormap,redMax,redMult,greenMax,greenMult,blueMax,blueMult,basePixel,visual}) =
    (
      (colormap,redMax,redMult,greenMax,greenMult,blueMax,blueMult,basePixel,visual)
    );
  in
    map Trans L
  end;
  
  fun XSetProperty (w:Drawable) (property:int) (v:PropertyValue) (t:int):unit =
  (
    case v of
      PropertyArc         L => xcall (XCALL_XSetProperty,w,property,t,L,XA_ARC)
    | PropertyAtom        L => xcall (XCALL_XSetProperty,w,property,t,L,XA_ATOM)
    | PropertyBitmap      L => xcall (XCALL_XSetProperty,w,property,t,L,XA_BITMAP)
    | PropertyColormap    L => xcall (XCALL_XSetProperty,w,property,t,L,XA_COLORMAP)
    | PropertyCursor      L => xcall (XCALL_XSetProperty,w,property,t,L,XA_CURSOR)
    | PropertyDrawable    L => xcall (XCALL_XSetProperty,w,property,t,L,XA_DRAWABLE)
    | PropertyFont        L => xcall (XCALL_XSetProperty,w,property,t,L,XA_FONT)
    | PropertyInteger     L => xcall (XCALL_XSetProperty,w,property,t,L,XA_INTEGER)
    | PropertyPixmap      L => xcall (XCALL_XSetProperty,w,property,t,L,XA_PIXMAP)
    | PropertyPoint       L => xcall (XCALL_XSetProperty,w,property,t,L,XA_POINT)
    | PropertyRectangle   L => xcall (XCALL_XSetProperty,w,property,t,L,XA_RECTANGLE)
    | PropertyString      s => xcall (XCALL_XSetProperty,w,property,t,s,XA_STRING)
    | PropertyVisual      L => xcall (XCALL_XSetProperty,w,property,t,L,XA_VISUALID)
    | PropertyWindow      L => xcall (XCALL_XSetProperty,w,property,t,L,XA_WINDOW)
    | PropertyRGBColormap L => xcall (XCALL_XSetProperty,w,property,t,fromXStandardColormaps L,XA_RGB_COLOR_MAP)
    | PropertyWMHints     L => xcall (XCALL_XSetProperty,w,property,t,[fromXWMHints L],XA_WM_HINTS)
    | PropertyWMSizeHints L => xcall (XCALL_XSetProperty,w,property,t,[fromXWMSizeHints L],XA_WM_SIZE_HINTS)
    | PropertyWMIconSizes L => xcall (XCALL_XSetProperty,w,property,t,L,XA_WM_ICON_SIZE)
  );
  
  fun XGetTextProperty (w:Drawable) (property:int):(string * int * int * int) =
    xcall (XCALL_XGetTextProperty,w,property);
  
  fun GetSet property exceptionName =
  let
    fun Get w = 
    let
      val (string,encoding,format,_) = XGetTextProperty w property;
    in
      if encoding = XA_STRING andalso format = 8 then string else raise XWindows exceptionName
    end;

    fun Set w s = XSetProperty w property (PropertyString s) XA_STRING;
  in
    (Get,Set)
  end;

  val (XGetWMName         ,XSetWMName)          = GetSet XA_WM_NAME           "XGetWMName";
  val (XGetWMIconName     ,XSetWMIconName)      = GetSet XA_WM_ICON_NAME      "XGetWMIconName";
  val (XGetWMClientMachine,XSetWMClientMachine) = GetSet XA_WM_CLIENT_MACHINE "XGetWMClientMachine";
  
  fun GetSetList property exceptionName =
  let
    fun Split s =
    let
      fun Accum acc s []     = rev (s::acc)
      |   Accum acc s (H::T) =
            if H = #"\000" then Accum (s::acc) "" T else Accum acc (s ^ String.str H) T;
    in
      Accum [] "" (explode s)
    end;
    
    fun Join []     = ""
    |   Join [s]    = s
    |   Join (H::T) = H ^ "\000" ^ Join T;
    
    val (Get,Set) = GetSet property exceptionName;
  in
    (fn w => Split (Get w),fn w => fn L => Set w (Join L))
  end;

  val (XGetWMCommand,XSetWMCommand) = GetSetList XA_WM_COMMAND "XGetWMCommand";
  val (XGetWMClass  ,XSetWMClass)   = GetSetList XA_WM_CLASS   "XGetWMClass";
  
  fun XSetWMHints w L = XSetProperty w XA_WM_HINTS (PropertyWMHints L) XA_WM_HINTS;
  
  fun XGetWMHints (win:Drawable) =
  let
    val (i,s,p,w,P,m,f) = xcall (XCALL_XGetWMHints,win);
    
    val s = toXWMStateHint s;
  in
    (if (f And 1)  <> 0 then [InputHint        i] else []) @
    (if (f And 2)  <> 0 then [StateHint        s] else []) @
    (if (f And 4)  <> 0 then [IconPixmapHint   p] else []) @
    (if (f And 8)  <> 0 then [IconWindowHint   w] else []) @
    (if (f And 16) <> 0 then [IconPositionHint P] else []) @
    (if (f And 32) <> 0 then [IconMaskHint     m] else [])
  end;

  fun XSetWMSizeHints w property L = XSetProperty w property (PropertyWMSizeHints L) XA_WM_SIZE_HINTS;
  
  fun XGetWMSizeHints (win:Drawable) (property:int) =
  let
    val (p,s,min,max,inc,a,bs,wg,f) = xcall (XCALL_XGetWMSizeHints,win,property);
    
    val wg = toGravity wg;
  in
    (if (f And 4)   <> 0 then [PPosition   p]   else []) @
    (if (f And 8)   <> 0 then [PSize       s]   else []) @
    (if (f And 16)  <> 0 then [PMinSize    min] else []) @
    (if (f And 32)  <> 0 then [PMaxSize    max] else []) @
    (if (f And 64)  <> 0 then [PResizeInc  inc] else []) @
    (if (f And 128) <> 0 then [PAspect     a]   else []) @
    (if (f And 256) <> 0 then [PBaseSize   bs]  else []) @
    (if (f And 512) <> 0 then [PWinGravity wg]  else [])
  end;
  
  fun XSetWMNormalHints w L = XSetWMSizeHints w XA_WM_NORMAL_HINTS L;
  fun XGetWMNormalHints w   = XGetWMSizeHints w XA_WM_NORMAL_HINTS;
  
  fun XSetWMProperties w windowName iconName commands normalHints wmHints class =
  (
    if windowName <> "" then XSetWMName w windowName else ();
    
    if iconName <> "" then XSetWMIconName w iconName else ();
    
    if commands <> [] then XSetWMCommand w commands else ();
    
    XSetWMNormalHints w normalHints;
    
    XSetWMHints w wmHints;
    
    if class <> [] then XSetWMClass w class else ()
  );
  
  fun XSetIconSizes w sizes = XSetProperty w XA_WM_ICON_SIZE (PropertyWMIconSizes sizes) XA_WM_ICON_SIZE;
  
  fun XGetIconSizes (w:Drawable):(XRectangle * XRectangle * XRectangle) list =
    xcall (XCALL_XGetIconSizes,w);
  
  fun XSetTransientForHint transient main = XSetProperty transient XA_WM_TRANSIENT_FOR (PropertyWindow [main]) XA_WINDOW;
  
  fun XGetTransientForHint (w:Drawable):Drawable =
    xcall (XCALL_XGetTransientForHint,w);
  
  fun XSetWMColormapWindows w subwins =
  let
    val property = XInternAtom (DrawableDisplay w) "WM_COLORMAP_WINDOWS" true;
  in
    if property = 0 then
      raise XWindows "XSetWMColormapWindows failed"
    else
      XSetProperty w property (PropertyWindow subwins) XA_WINDOW
  end;
  
  fun XGetWMColormapWindows (w:Drawable):Drawable list =
    xcall (XCALL_XGetWMColormapWindows,w);
  
  fun XSetRGBColormaps w property L = XSetProperty w property (PropertyRGBColormap L) XA_RGB_COLOR_MAP;
  
  fun XGetRGBColormaps (w:Drawable) (property:int) =
  let
    fun Trans (colormap,redMax,redMult,greenMax,greenMult,blueMax,blueMult,basePixel,visual) =
    (
      XStandardColormap { colormap  = colormap,
                          redMax    = redMax,
                          redMult   = redMult,
                          greenMax  = greenMax,
                          greenMult = greenMult,
                          blueMax   = blueMax,
                          blueMult  = blueMult,
                          basePixel = basePixel,
                          visual    = visual }
    );
  in
    map Trans (xcall (XCALL_XGetRGBColormaps,w,property))
  end;
  
  fun XWMGeometry (d:Display) (user:string) (def:string) (borderWidth:int) (L:XWMSizeHint list):(XPoint * XRectangle * Gravity) =
  let
    val (p,r,g) = xcall (XCALL_XWMGeometry,d,user,def,borderWidth,fromXWMSizeHints L);
  in
    (p,r,toGravity g)
  end;

  (* Miscellaneous and Convenience functions 1300 *)
  
  local
    fun GetID x = xcall (XCALL_GetID,x):int;
  in
    val ColormapID = GetID : Colormap -> int;
    val CursorID   = GetID : Cursor   -> int;
    val DrawableID = GetID : Drawable -> int;
    val FontID     = GetID : Font     -> int;
    val GCID       = GetID : GC       -> int;
    val VisualID   = GetID : Visual   -> int;
  end;
  
  fun SameDrawable a b = (DrawableID a = DrawableID b);
  
  fun XSetColors gc fg bg = (XSetForeground gc fg; XSetBackground gc bg);

  fun XGetWindowRoot        w = #1 (XGetGeometry w);
  fun XGetWindowPosition    w = #2 (XGetGeometry w);
  fun XGetWindowSize        w = #3 (XGetGeometry w);
  fun XGetWindowBorderWidth w = #4 (XGetGeometry w);
  fun XGetWindowDepth       w = #5 (XGetGeometry w);
  fun XGetWindowParent      w = #2 (XQueryTree w);
  fun XGetWindowChildren    w = #3 (XQueryTree w);
  
  fun GetTimeOfDay():(int * int) = xcall (XCALL_GetTimeOfDay,0);

  fun FSFont          (XFontStruct {font         ,...}) = font;
  fun FSDirection     (XFontStruct {direction    ,...}) = direction;
  fun FSMinChar       (XFontStruct {minChar      ,...}) = minChar;
  fun FSMaxChar       (XFontStruct {maxChar      ,...}) = maxChar;
  fun FSMinByte1      (XFontStruct {minByte1     ,...}) = minByte1;
  fun FSMaxByte1      (XFontStruct {maxByte1     ,...}) = maxByte1;
  fun FSAllCharsExist (XFontStruct {allCharsExist,...}) = allCharsExist;
  fun FSDefaultChar   (XFontStruct {defaultChar  ,...}) = defaultChar;
  fun FSMinBounds     (XFontStruct {minBounds    ,...}) = minBounds;
  fun FSMaxBounds     (XFontStruct {maxBounds    ,...}) = maxBounds;
  fun FSPerChar       (XFontStruct {perChar      ,...}) = perChar;
  fun FSAscent        (XFontStruct {ascent       ,...}) = ascent;
  fun FSDescent       (XFontStruct {descent      ,...}) = descent;
  
  fun CharLBearing   (XCharStruct {lbearing  ,...}) = lbearing;
  fun CharRBearing   (XCharStruct {rbearing  ,...}) = rbearing;
  fun CharWidth      (XCharStruct {width     ,...}) = width;
  fun CharAscent     (XCharStruct {ascent    ,...}) = ascent;
  fun CharDescent    (XCharStruct {descent   ,...}) = descent;
  fun CharAttributes (XCharStruct {attributes,...}) = attributes;
  
  fun FSMinWidth  f = CharWidth  (FSMinBounds f);
  fun FSMaxWidth  f = CharWidth  (FSMaxBounds f);
  fun FSMinHeight f = CharAscent (FSMinBounds f) + CharDescent (FSMinBounds f);
  fun FSMaxHeight f = CharAscent (FSMaxBounds f) + CharDescent (FSMaxBounds f);
  
  fun ShiftDown   [] = false | ShiftDown   (ShiftMask::_)   = true | ShiftDown   (_::T) = ShiftDown   T;
  fun ControlDown [] = false | ControlDown (ControlMask::_) = true | ControlDown (_::T) = ControlDown T;
  
end (* local *);

end (* structure XWindows *);

exception XWindows = XWindows.XWindows;

local
  open XWindows;
  
  fun printRect _ _ (r:XRectangle) =
  let
    val R = DestructRect r;
  in
    PolyML.PrettyString ("Rect " ^ PolyML.makestring R)
  end;
in
  val () = PolyML.addPrettyPrinter printRect
end;

