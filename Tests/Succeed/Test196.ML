(* Check that fullPath works and that the empty path is treated as "." *)
if (OS.FileSys.fullPath "" = OS.FileSys.fullPath ".") then () else raise Fail "fullpath";
