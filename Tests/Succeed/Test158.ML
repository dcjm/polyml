(* Various tests for the OS.Path structure.  The Unix tests are taken largely
   from the documentation.  The Windows tests are inferred from the documentation
   which isn't always clear. *)

datatype sys = Windows | Posix;

open OS.Path;

fun check s true = () | check s false = raise Fail ("check " ^ s);

val check' = check "";

val sys = if isRoot "/" then Posix else Windows;

(* Windows uses either / or \ as a separator but the
   output routines generally produce \.  To simplify the
   checks map backslashes into forward slashes in some cases. *)
val mapSep =
    case sys of
        Posix => (fn c => c)
    |   Windows => String.map (fn #"\\" => #"/" | c => c);

(* fromString *)
val testFromTo =
[
    {
        s = "",
        p = {isAbs=false, vol="", arcs=[]},
        w = {isAbs=false, vol="", arcs=[]}
    },
    {
        s = "/",
        p = {isAbs=true, vol="", arcs=[""]},
        (* In Windows this means the "root" directory on the current drive. *)
        w = {isAbs = false, vol = "", arcs = ["", ""]}
    },
    {
        s = "//",
        p = {isAbs=true, vol="", arcs=["", ""]},
        w = {isAbs = false, vol = "", arcs = ["", "", ""]}
    },
    {
        s = "a",
        p = {isAbs=false, vol="", arcs=["a"]},
        w = {isAbs=false, vol="", arcs=["a"]}
    },
    {
        s = "/a",
        p = {isAbs=true, vol="", arcs=["a"]},
        w = {isAbs=false, vol="", arcs=["", "a"]}
    },
    {
        s = "//a",
        p = {isAbs=true, vol="", arcs=["","a"]},
        w = {isAbs=false, vol="", arcs=["","", "a"]}
    },
    {
        s = "a/",
        p = {isAbs=false, vol="", arcs=["a", ""]},
        w = {isAbs=false, vol="", arcs=["a", ""]}
    },
    {
        s = "a//",
        p = {isAbs=false, vol="", arcs=["a", "", ""]},
        w = {isAbs=false, vol="", arcs=["a", "", ""]}
    },
    {
        s = "a/b",
        p = {isAbs=false, vol="", arcs=["a", "b"]},
        w = {isAbs=false, vol="", arcs=["a", "b"]}
    },
    {
        s = "c:\\",
        p = {isAbs=false, vol="", arcs=["c:\\"]},
        w = {isAbs = true, vol = "c:", arcs = [""]}
    },
    {
        s = "c:",
        p = {isAbs=false, vol="", arcs=["c:"]},
        w = {isAbs=false, vol = "c:", arcs = [""]}
    },
    {
        s = "c:\\abc",
        p = {isAbs=false, vol="", arcs=["c:\\abc"]},
        w = {isAbs=true, vol = "c:", arcs = ["abc"]}
    }


];

fun test { s, p, w } =
let
    val res = fromString s
    val ts = toString res
in
    case sys of
        Posix => check s (res = p)
    |   Windows => check s (res = w);
    check s (ts = s orelse mapSep ts = s)
end;

val () = List.app test testFromTo;

(* validVolume *)
val v1 = validVolume{isAbs = true, vol = ""};
case sys of Posix => check' v1 | Windows => check' (not v1);

val v2 = validVolume{isAbs = false, vol = ""};
check' v2;

val v3 = validVolume{isAbs = true, vol = "C:"};
case sys of Posix => check'(not v3) | Windows => check' v3;

val v4 = validVolume{isAbs = false, vol = "C:"};
case sys of Posix => check'(not v4) | Windows => check' v4;

val v5 = validVolume{isAbs = false, vol = "\\\\server\\share" };
case sys of Posix => check'(not v5) | Windows => check' v5;


(* getParent *)
val testGetParent =
[
    { s = "/", p = "/", w = "/" },
    { s = "a", p = ".", w = "." },
    { s = "a/", p = "a/..", w = "a/.." },
    { s = "a///", p = "a///..", w = "a///.." },
    { s = "a/b", p = "a", w = "a" },
    { s = "a/b/", p = "a/b/..", w = "a/b/.." },
    { s = "..", p = "../..", w = "../.." },
    { s = ".", p = "..", w = ".." },
    { s = "C:\\", p = ".", w = "C:\\" },
    { s = "\\\\server\\share\\", p = ".", w = "\\\\server\\share\\" }
];

fun test { s, p, w } =
let
    val res = getParent s
in
    case sys of
        Posix => check s (res = p)
    |   Windows => check s (res = w orelse mapSep res = mapSep w)
end;

val () = List.app test testGetParent;


(* splitDirFile *)
val testSplitFile =
[
    {
        s = "",
        p = {dir = "", file = ""},
        w = {dir = "", file = ""}
    },
    {
        s = ".",
        p = {dir = "", file = "."},
        w = {dir = "", file = "."}
    },
    {
        s = "b",
        p = {dir = "", file = "b"},
        w = {dir = "", file = "b"}
    },
    {
        s = "b/",
        p = {dir = "b", file = ""},
        w = {dir = "b", file = ""}
    },
    {
        s = "a/b",
        p = {dir = "a", file = "b"},
        w = {dir = "a", file = "b"}
    },
    {
        s = "/a",
        p = {dir = "/", file = "a"},
        w = {dir = "/", file = "a"}
    },
    {
        s = "c:\\a",
        p = {dir = "", file = "c:\\a"},
        w = {dir = "c:\\", file = "a"}
    }

];

fun test { s, p, w } =
let
    val res as { dir, file } = splitDirFile s
in
    case sys of
        Posix => check s (res = p)
    |   Windows => check s (res = w orelse mapSep dir = #dir w andalso mapSep file = #file w)
end;

val () = List.app test testSplitFile;

val testAbsolute =
[
    { s = "/a", p = true, w = false },
    { s = "c:/a", p = false, w = true }
];

fun test { s, p, w } =
let
    val res = isAbsolute s
in
    case sys of
        Posix => check s (res = p)
    |   Windows => check s (res = w)
end;

val () = List.app test testAbsolute;

case sys of
    Posix =>
    let
        val testMkRelative =
        [
            (* These are all relative paths in Windows. *)
            { s = { path = "a/b",   relativeTo = "/c/d" },      p = "a/b"},
            { s = { path = "/",     relativeTo = "/a/b/c" },    p = "../../.." },
            { s = { path = "/a/b/", relativeTo = "/a/c" },      p = "../b/" },
            { s = { path = "/a/b",  relativeTo = "/a/c" },      p = "../b" },
            { s = { path = "/a/b/", relativeTo = "/a/c/"},      p = "../b/" },
            { s = { path = "/a/b",  relativeTo = "/a/c/"},      p = "../b" },
            { s = { path = "/",     relativeTo = "/"},          p = "." },
            { s = { path = "/",     relativeTo = "/."},         p = "." },
            { s = { path = "/",     relativeTo = "/.."},        p = "." },
            { s = { path = "/a/b/../c", relativeTo = "/a/d"},   p = "../b/../c" },
            { s = { path = "/a/b",      relativeTo = "/c/d"},   p = "../../a/b" },
            { s = { path = "/c/a/b",    relativeTo =  "/c/d"},  p = "../a/b" },
            { s = { path = "/c/d/a/b",  relativeTo =  "/c/d"},  p = "a/b" }
        ];

        fun test { s, p } =
        let
            val res = mkRelative s
        in
            check (#path s ^ " " ^ #relativeTo s) (res = p)
        end;
    in
        List.app test testMkRelative
    end
|   Windows =>
    let
        val testMkRelative =
        [
            { s = { path = "a/b",   relativeTo = "c:/c/d" },        w = "a/b" },
            { s = { path = "c:/",     relativeTo = "c:/a/b/c" },    w = "../../.." },
            { s = { path = "c:/a/b/", relativeTo = "c:/a/c" },      w = "../b/" },
            { s = { path = "c:/a/b",  relativeTo = "c:/a/c" },      w = "../b" },
            { s = { path = "c:/a/b/", relativeTo = "c:/a/c/"},      w = "../b/" },
            { s = { path = "c:/a/b",  relativeTo = "c:/a/c/"},      w = "../b" },
            { s = { path = "c:/",     relativeTo = "c:/"},          w = "." },
            { s = { path = "c:/",     relativeTo = "c:/."},         w = "." },
            { s = { path = "c:/",     relativeTo = "c:/.."},        w = "." },
            { s = { path = "c:/a/b/../c", relativeTo = "c:/a/d"},   w = "../b/../c" },
            { s = { path = "c:/a/b",      relativeTo = "c:/c/d"},   w = "../../a/b" },
            { s = { path = "c:/c/a/b",    relativeTo =  "c:/c/d"},  w = "../a/b" },
            { s = { path = "c:/c/d/a/b",  relativeTo =  "c:/c/d"},  w = "a/b" } 
        ];

        fun test { s, w } =
        let
            val res = mkRelative s
        in
            check (#path s ^ " " ^ #relativeTo s) (res = w orelse mapSep res = mapSep w)
        end;
    in
        List.app test testMkRelative
    end;

case sys of
    Windows =>
    (
        (* Special cases for the relative path \\abc.  This is relative to the current volume. *)
        check "" (mkAbsolute{path="\\abc", relativeTo="c:\\def\\xyz"} = "c:\\abc");
        check "" (concat("c:\\abc\\def", "\\ghi") = "c:\\ghi")
    )
|   Posix => ();

(* OS.Path.joinDirFile should raise InvalidArc if the file name does not correspond to an arc *)
(OS.Path.joinDirFile {dir="abc/def", file="ghi/jkl"}; raise Fail "incorrect") handle OS.Path.InvalidArc => ();

(* Extra test added as a result of issue #167 *)
local
    val p1 = getParent currentArc
    val p2 = getParent p1
    val p3 = getParent p2
in
    val () = check "parent" (p1 = parentArc)
    val () = check "parent-parent" (p2 = (case sys of Windows => "..\\.." | Posix => "../.."))
    val () = check "parent-parent-parent" (p3 = (case sys of Windows => "..\\..\\.." | Posix => "../../.."))
end;

