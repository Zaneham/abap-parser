(* Source location tracking *)

type pos = {
  line : int;
  col  : int;
}

type t = {
  start  : pos;
  finish : pos;
  file   : string;
}

let dummy = {
  start  = { line = 0; col = 0 };
  finish = { line = 0; col = 0 };
  file   = "<unknown>";
}

let pp fmt loc =
  Format.fprintf fmt "%s:%d:%d" loc.file loc.start.line loc.start.col
