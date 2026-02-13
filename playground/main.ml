(* js_of_ocaml entry point for the browser playground. *)

let parse_abap src =
  try
    let tokens = Abap.Lexer.tokenize ~file:"<playground>" src in
    let prog = Abap.Parser.parse_program tokens in
    let buf = Buffer.create 4096 in
    let fmt = Format.formatter_of_buffer buf in
    Abap.Printer.pp_program fmt prog;
    Format.pp_print_flush fmt ();
    Buffer.contents buf
  with Failure msg -> "Error: " ^ msg
     | exn -> "Error: " ^ Printexc.to_string exn

let () =
  Js_of_ocaml.Js.export "abapParse"
    (object%js
       method parse (src : Js_of_ocaml.Js.js_string Js_of_ocaml.Js.t) =
         Js_of_ocaml.Js.string (parse_abap (Js_of_ocaml.Js.to_string src))
     end)
