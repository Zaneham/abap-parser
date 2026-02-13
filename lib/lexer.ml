(* Hand-rolled ABAP lexer.

   Lexical rules:
   - Case insensitive: identifiers uppercased on emission
   - Keywords NOT reserved: everything is IDENT, parser disambiguates
   - Period terminates every statement
   - Colon introduces chains, comma separates chain parts
   - Comments: * in column 1 = full-line, double-quote = rest-of-line
   - Text literals: 'single quoted' (type C), '' for embedded quote
   - Text strings: `backtick quoted` (type STRING), `` for embedded backtick
   - String templates: |text {expr} text| (7.40+), mode switching
   - Identifiers may contain hyphens: sy-subrc, CLASS-DATA, END-OF-SELECTION
   - Field symbols: <name>
   - ! escapes an identifier from keyword interpretation
   - No float literals: period is always statement terminator *)

type mode =
  | Normal
  | InTemplate

type t = {
  src  : string;
  file : string;
  len  : int;
  mutable pos  : int;
  mutable lno  : int;
  mutable cno  : int;
  mutable mode_stack : mode list;
  mutable saw_ws : bool;
}

let create ~file src =
  { src; file; len = String.length src;
    pos = 0; lno = 1; cno = 1;
    mode_stack = [Normal];
    saw_ws = true }

(* -- helpers ------------------------------------------------------------ *)

let cur t = if t.pos < t.len then t.src.[t.pos] else '\000'

let advance t =
  if t.pos < t.len then begin
    if t.src.[t.pos] = '\n' then begin
      t.lno <- t.lno + 1;
      t.cno <- 1
    end else
      t.cno <- t.cno + 1;
    t.pos <- t.pos + 1
  end

let loc t sl sc =
  { Loc.start  = { Loc.line = sl; col = sc };
    finish = { Loc.line = t.lno; col = t.cno };
    file   = t.file }

let current_mode t =
  match t.mode_stack with m :: _ -> m | [] -> Normal

let push_mode t m = t.mode_stack <- m :: t.mode_stack

let pop_mode t =
  match t.mode_stack with _ :: rest -> t.mode_stack <- rest | [] -> ()

let is_letter c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
let is_digit  c = c >= '0' && c <= '9'
let is_ident_char c = is_letter c || is_digit c || c = '_'

let skip_ws t =
  t.saw_ws <- false;
  let rec loop () =
    if t.pos < t.len then
      match t.src.[t.pos] with
      | ' ' | '\t' | '\r' | '\n' -> t.saw_ws <- true; advance t; loop ()
      | _ -> ()
  in
  loop ()

(* -- comment readers ---------------------------------------------------- *)

(* Full-line comment: * at column 1, read to end of line *)
let read_line_comment t =
  let sl = t.lno and sc = t.cno in
  advance t;
  let start = t.pos in
  while t.pos < t.len && t.src.[t.pos] <> '\n' do advance t done;
  let text = String.sub t.src start (t.pos - start) in
  (Token.COMMENT text, loc t sl sc)

(* Inline comment: double-quote to end of line *)
let read_inline_comment t =
  let sl = t.lno and sc = t.cno in
  advance t;
  let start = t.pos in
  while t.pos < t.len && t.src.[t.pos] <> '\n' do advance t done;
  let text = String.sub t.src start (t.pos - start) in
  (Token.COMMENT text, loc t sl sc)

(* -- literal readers ---------------------------------------------------- *)

(* Single-quoted text literal: 'text', '' for embedded quote *)
let read_string t =
  let sl = t.lno and sc = t.cno in
  advance t;
  let buf = Buffer.create 64 in
  let rec loop () =
    if t.pos >= t.len then
      failwith (Printf.sprintf "%s:%d:%d: unterminated string literal"
        t.file sl sc);
    let c = t.src.[t.pos] in
    if c = '\'' then begin
      advance t;
      if t.pos < t.len && t.src.[t.pos] = '\'' then begin
        Buffer.add_char buf '\''; advance t; loop ()
      end
    end else begin
      Buffer.add_char buf c; advance t; loop ()
    end
  in
  loop ();
  (Token.STRING (Buffer.contents buf), loc t sl sc)

(* Backtick-quoted text string: `text`, `` for embedded backtick *)
let read_bstring t =
  let sl = t.lno and sc = t.cno in
  advance t;
  let buf = Buffer.create 64 in
  let rec loop () =
    if t.pos >= t.len then
      failwith (Printf.sprintf "%s:%d:%d: unterminated backtick string"
        t.file sl sc);
    let c = t.src.[t.pos] in
    if c = '`' then begin
      advance t;
      if t.pos < t.len && t.src.[t.pos] = '`' then begin
        Buffer.add_char buf '`'; advance t; loop ()
      end
    end else begin
      Buffer.add_char buf c; advance t; loop ()
    end
  in
  loop ();
  (Token.BSTRING (Buffer.contents buf), loc t sl sc)

(* Numeric literal: digits only, sign handled as separate operator *)
let read_number t =
  let sl = t.lno and sc = t.cno in
  let start = t.pos in
  while t.pos < t.len && is_digit t.src.[t.pos] do advance t done;
  let text = String.sub t.src start (t.pos - start) in
  (Token.INT text, loc t sl sc)

(* -- identifier reader -------------------------------------------------- *)

(* Identifier: letter/underscore followed by letters/digits/underscores,
   with optional hyphenated segments (sy-subrc, CLASS-DATA, END-OF-SELECTION).
   Hyphen only continues if the character after it is a letter or _. *)
let read_ident t =
  let sl = t.lno and sc = t.cno in
  let buf = Buffer.create 32 in
  let read_word () =
    while t.pos < t.len && is_ident_char t.src.[t.pos] do
      Buffer.add_char buf t.src.[t.pos]; advance t
    done
  in
  read_word ();
  let rec check_separator () =
    if t.pos < t.len then
      match t.src.[t.pos] with
      | '-' | '~' ->
        let nxt = t.pos + 1 in
        if nxt < t.len && (is_letter t.src.[nxt] || t.src.[nxt] = '_') then begin
          Buffer.add_char buf t.src.[t.pos]; advance t;
          read_word ();
          check_separator ()
        end
      | _ -> ()
  in
  check_separator ();
  let name = String.uppercase_ascii (Buffer.contents buf) in
  (Token.IDENT name, loc t sl sc)

(* -- field symbol reader ------------------------------------------------ *)

(* Field symbol: <name>, where name is an identifier *)
let read_field_symbol t =
  let sl = t.lno and sc = t.cno in
  advance t;  (* skip < *)
  let buf = Buffer.create 32 in
  while t.pos < t.len && t.src.[t.pos] <> '>' && t.src.[t.pos] <> '\n' do
    Buffer.add_char buf t.src.[t.pos]; advance t
  done;
  if t.pos < t.len && t.src.[t.pos] = '>' then
    advance t
  else
    failwith (Printf.sprintf "%s:%d:%d: unterminated field symbol"
      t.file sl sc);
  (Token.FIELD_SYMBOL (String.uppercase_ascii (Buffer.contents buf)),
   loc t sl sc)

(* -- namespaced identifier reader --------------------------------------- *)

(* Peek ahead to check if /letters.../ pattern follows (no state mutation) *)
let peek_namespace t =
  let p = ref (t.pos + 1) in
  if !p >= t.len || not (is_letter t.src.[!p]) then false
  else begin
    while !p < t.len && is_ident_char t.src.[!p] do incr p done;
    !p < t.len && t.src.[!p] = '/'
  end

(* Read /NAMESPACE/name as a single identifier *)
let read_namespaced_ident t =
  let sl = t.lno and sc = t.cno in
  let buf = Buffer.create 32 in
  Buffer.add_char buf '/'; advance t;
  (* namespace part *)
  while t.pos < t.len && is_ident_char t.src.[t.pos] do
    Buffer.add_char buf t.src.[t.pos]; advance t
  done;
  (* closing / *)
  if t.pos < t.len && t.src.[t.pos] = '/' then begin
    Buffer.add_char buf '/'; advance t
  end;
  (* name part *)
  while t.pos < t.len && is_ident_char t.src.[t.pos] do
    Buffer.add_char buf t.src.[t.pos]; advance t
  done;
  (* hyphen/tilde continuations *)
  let rec check_sep () =
    if t.pos < t.len then
      match t.src.[t.pos] with
      | '-' | '~' ->
        let nxt = t.pos + 1 in
        if nxt < t.len && (is_letter t.src.[nxt] || t.src.[nxt] = '_') then begin
          Buffer.add_char buf t.src.[t.pos]; advance t;
          while t.pos < t.len && is_ident_char t.src.[t.pos] do
            Buffer.add_char buf t.src.[t.pos]; advance t
          done;
          check_sep ()
        end
      | _ -> ()
  in
  check_sep ();
  let name = String.uppercase_ascii (Buffer.contents buf) in
  (Token.IDENT name, loc t sl sc)

(* -- string template reader --------------------------------------------- *)

(* Read literal text inside a string template until { or | *)
let read_template_text t =
  let sl = t.lno and sc = t.cno in
  let buf = Buffer.create 64 in
  let rec loop () =
    if t.pos >= t.len then
      failwith (Printf.sprintf "%s:%d:%d: unterminated string template"
        t.file sl sc);
    match t.src.[t.pos] with
    | '{' | '|' -> ()
    | '\\' ->
      advance t;
      if t.pos < t.len then begin
        let c = match t.src.[t.pos] with
          | 'n' -> '\n' | 't' -> '\t' | '\\' -> '\\'
          | '{' -> '{' | '}' -> '}' | '|' -> '|'
          | c -> c
        in
        Buffer.add_char buf c; advance t; loop ()
      end
    | c ->
      Buffer.add_char buf c; advance t; loop ()
  in
  loop ();
  (Token.TEMPLATE_TEXT (Buffer.contents buf), loc t sl sc)

(* -- main tokenizer ----------------------------------------------------- *)

let rec next t =
  match current_mode t with
  | InTemplate ->
    let (tok, l) = next_template t in
    (tok, l, false)
  | Normal ->
    let (tok, l) = next_normal t in
    (tok, l, t.saw_ws)

and next_template t =
  if t.pos >= t.len then
    failwith (Printf.sprintf "%s: unexpected end of file in string template"
      t.file);
  match t.src.[t.pos] with
  | '{' ->
    let sl = t.lno and sc = t.cno in
    advance t;
    push_mode t Normal;
    (Token.TEMPLATE_EXPR_BEGIN, loc t sl sc)
  | '|' ->
    let sl = t.lno and sc = t.cno in
    advance t;
    pop_mode t;
    (Token.TEMPLATE_END, loc t sl sc)
  | _ ->
    read_template_text t

and next_normal t =
  skip_ws t;
  if t.pos >= t.len then
    (Token.EOF, loc t t.lno t.cno)
  else
    let sl = t.lno and sc = t.cno in
    match t.src.[t.pos] with
    (* Full-line comment: * at column 1 *)
    | '*' when sc = 1 -> read_line_comment t

    (* Inline comment *)
    | '"' -> read_inline_comment t

    (* String literals *)
    | '\'' -> read_string t
    | '`'  -> read_bstring t

    (* String template *)
    | '|' ->
      advance t;
      push_mode t InTemplate;
      (Token.TEMPLATE_BEGIN, loc t sl sc)

    (* Close template expression *)
    | '}' ->
      advance t;
      pop_mode t;
      (Token.TEMPLATE_EXPR_END, loc t sl sc)

    (* Numeric literal *)
    | '0'..'9' -> read_number t

    (* Escaped identifier: !name *)
    | '!' ->
      advance t;
      if t.pos < t.len && (is_letter (cur t) || cur t = '_') then
        read_ident t
      else
        failwith (Printf.sprintf "%s:%d:%d: expected identifier after '!'"
          t.file sl sc)

    (* Identifier *)
    | c when is_letter c || c = '_' -> read_ident t

    (* < : field symbol, <>, <=, or LT *)
    | '<' ->
      let nxt = t.pos + 1 in
      if nxt < t.len then
        match t.src.[nxt] with
        | c when is_letter c || c = '_' -> read_field_symbol t
        | '>' -> advance t; advance t; (Token.NE, loc t sl sc)
        | '=' -> advance t; advance t; (Token.LE, loc t sl sc)
        | _   -> advance t; (Token.LT, loc t sl sc)
      else begin
        advance t; (Token.LT, loc t sl sc)
      end

    (* > : >= or GT *)
    | '>' ->
      advance t;
      if t.pos < t.len && t.src.[t.pos] = '=' then begin
        advance t; (Token.GE, loc t sl sc)
      end else
        (Token.GT, loc t sl sc)

    (* = : => or EQ *)
    | '=' ->
      advance t;
      if t.pos < t.len && t.src.[t.pos] = '>' then begin
        advance t; (Token.DARROW, loc t sl sc)
      end else
        (Token.EQ, loc t sl sc)

    (* - : -> or MINUS *)
    | '-' ->
      advance t;
      if t.pos < t.len && t.src.[t.pos] = '>' then begin
        advance t; (Token.ARROW, loc t sl sc)
      end else
        (Token.MINUS, loc t sl sc)

    (* + *)
    | '+' -> advance t; (Token.PLUS, loc t sl sc)

    (* * : ** or STAR (not comment here, that case was caught above) *)
    | '*' ->
      advance t;
      if t.pos < t.len && t.src.[t.pos] = '*' then begin
        advance t; (Token.POWER, loc t sl sc)
      end else
        (Token.STAR, loc t sl sc)

    (* / : division or /namespace/name *)
    | '/' ->
      if peek_namespace t then
        read_namespaced_ident t
      else begin
        advance t; (Token.SLASH, loc t sl sc)
      end

    (* & : && or single & *)
    | '&' ->
      advance t;
      if t.pos < t.len && t.src.[t.pos] = '&' then begin
        advance t; (Token.AMPAMP, loc t sl sc)
      end else
        (Token.AMP, loc t sl sc)

    (* Simple single-character tokens *)
    | '.' -> advance t; (Token.DOT,      loc t sl sc)
    | ',' -> advance t; (Token.COMMA,    loc t sl sc)
    | ':' -> advance t; (Token.COLON,    loc t sl sc)
    | '(' -> advance t; (Token.LPAREN,   loc t sl sc)
    | ')' -> advance t; (Token.RPAREN,   loc t sl sc)
    | '[' -> advance t; (Token.LBRACKET, loc t sl sc)
    | ']' -> advance t; (Token.RBRACKET, loc t sl sc)
    | '@' -> advance t; (Token.AT,       loc t sl sc)
    | '#' ->
      if t.pos + 1 < t.len && t.src.[t.pos + 1] = '#' then begin
        advance t; advance t;
        let start = t.pos in
        while t.pos < t.len && is_ident_char t.src.[t.pos] do advance t done;
        let name = String.uppercase_ascii
          (String.sub t.src start (t.pos - start)) in
        (Token.PRAGMA name, loc t sl sc)
      end else begin
        advance t; (Token.HASH, loc t sl sc)
      end
    | '~' -> advance t; (Token.TILDE,    loc t sl sc)

    (* ?= casting assignment *)
    | '?' ->
      advance t;
      if t.pos < t.len && t.src.[t.pos] = '=' then begin
        advance t; (Token.CAST_EQ, loc t sl sc)
      end else
        failwith (Printf.sprintf "%s:%d:%d: unexpected '?' (expected '?=')"
          t.file sl sc)

    (* Unknown *)
    | c ->
      failwith (Printf.sprintf "%s:%d:%d: unexpected character '%c' (0x%02X)"
        t.file sl sc c (Char.code c))

(* Tokenize entire source, returning list of (token, loc) pairs *)
let tokenize ~file src =
  let lexer = create ~file src in
  let rec loop acc =
    let (tok, l, ws) = next lexer in
    match tok with
    | Token.EOF -> List.rev ((tok, l, ws) :: acc)
    | _ -> loop ((tok, l, ws) :: acc)
  in
  loop []
