(* ABAP parser CLI *)

type mode = Lex | Parse | Dump

let () =
  let mode = ref Parse in
  let files = ref [] in
  Array.iteri (fun i arg ->
    if i = 0 then ()  (* skip program name *)
    else match arg with
    | "--lex"   -> mode := Lex
    | "--parse" -> mode := Parse
    | "--dump"  -> mode := Dump
    | s -> files := s :: !files
  ) Sys.argv;
  let files = List.rev !files in
  if files = [] then begin
    Printf.eprintf "usage: abap-parse [--lex|--parse|--dump] <file.abap>\n";
    exit 1
  end;
  List.iter (fun file ->
    let src = In_channel.with_open_text file In_channel.input_all in
    let tokens = Abap.Lexer.tokenize ~file src in
    match !mode with
    | Lex ->
      List.iter (fun (tok, l, _ws) ->
        Format.printf "%a  %a\n" Abap.Loc.pp l Abap.Token.pp tok
      ) tokens
    | Parse | Dump ->
      let prog = Abap.Parser.parse_program tokens in
      Abap.Printer.pp_program Format.std_formatter prog
  ) files
