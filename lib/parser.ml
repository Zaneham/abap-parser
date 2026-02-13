(* Recursive descent ABAP parser with Pratt expression parsing.

   ABAP is what happens when you give COBOL a German passport and
   forty years of backwards compatibility. Keywords aren't reserved,
   periods end statements like stern punctuation, colons introduce
   chain statements, and whitespace before a parenthesis is the
   difference between calling a function and merely thinking about it.

   Unknown statements are collected into SUnknown rather than crashing,
   because in ABAP the unknown is more of a lifestyle than an error. *)

(* ---- parser state ------------------------------------------------------ *)

type t = {
  tokens : (Token.t * Loc.t * bool) array;   (* tok, loc, ws_before *)
  mutable pos : int;
}

let create tokens =
  { tokens = Array.of_list tokens; pos = 0 }

(* ---- navigation -------------------------------------------------------- *)

let cur t =
  if t.pos < Array.length t.tokens then
    let (tok, _, _) = t.tokens.(t.pos) in tok
  else Token.EOF

let cur_loc t =
  if t.pos < Array.length t.tokens then
    let (_, l, _) = t.tokens.(t.pos) in l
  else Loc.dummy

let cur_ws t =
  if t.pos < Array.length t.tokens then
    let (_, _, ws) = t.tokens.(t.pos) in ws
  else true

let advance t =
  if t.pos < Array.length t.tokens then
    t.pos <- t.pos + 1

let peek t n =
  let i = t.pos + n in
  if i < Array.length t.tokens then
    let (tok, _, _) = t.tokens.(i) in tok
  else Token.EOF

let peek_ws t n =
  let i = t.pos + n in
  if i < Array.length t.tokens then
    let (_, _, ws) = t.tokens.(i) in ws
  else true

(* ---- errors ------------------------------------------------------------ *)

let error t msg =
  let loc = cur_loc t in
  let tok_str = Format.asprintf "%a" Token.pp (cur t) in
  failwith (Printf.sprintf "%s:%d:%d: %s (got %s)"
    loc.file loc.start.line loc.start.col msg tok_str)

let expect t tok =
  if cur t = tok then
    (let l = cur_loc t in advance t; l)
  else
    let expected = Format.asprintf "%a" Token.pp tok in
    error t (Printf.sprintf "expected %s" expected)

let expect_dot t = ignore (expect t Token.DOT)

(* ---- keyword helpers --------------------------------------------------- *)

let is_kw_tok tok name =
  match tok with Token.IDENT s -> String.equal s name | _ -> false

let is_kw t name = is_kw_tok (cur t) name

let expect_kw t name =
  if is_kw t name then
    (let l = cur_loc t in advance t; l)
  else
    error t (Printf.sprintf "expected keyword %s" name)

let cur_ident t =
  match cur t with Token.IDENT s -> s | _ -> error t "expected identifier"

let expect_ident t =
  match cur t with
  | Token.IDENT s -> let l = cur_loc t in advance t; (s, l)
  | _ -> error t "expected identifier"

(* Skip tokens until we hit any of the given terminators *)
let skip_to t stops =
  while not (List.mem (cur t) stops) && cur t <> Token.EOF do
    advance t
  done

(* ---- Pratt precedence -------------------------------------------------- *)

let prec_or     = 10
let prec_and    = 20
let prec_cmp    = 40
let prec_concat = 50
let prec_add    = 60
let prec_mul    = 70
let prec_power  = 80
let prec_post   = 100

(* Infix binding power. Returns None for tokens that aren't infix.
   The bool is true for right-associative operators. *)
let infix_info t =
  match cur t with
  | Token.IDENT "OR"  -> Some (prec_or,  false)
  | Token.IDENT "AND" -> Some (prec_and, false)
  | Token.EQ when
      (match peek t 1 with
       | Token.IDENT _ | Token.INT _ | Token.FLOAT _
       | Token.STRING _ | Token.BSTRING _ | Token.LPAREN
       | Token.FIELD_SYMBOL _ | Token.TEMPLATE_BEGIN | Token.HASH -> true
       | _ -> false) ->
    Some (prec_cmp, false)
  | Token.NE | Token.LT | Token.GT | Token.LE | Token.GE ->
    Some (prec_cmp, false)
  | Token.IDENT ("EQ"|"NE"|"LT"|"GT"|"LE"|"GE"
                 |"CO"|"CN"|"CA"|"NA"|"CS"|"NS"|"CP"|"NP") ->
    Some (prec_cmp, false)
  | Token.IDENT "IN" -> Some (prec_cmp, false)
  | Token.AMPAMP -> Some (prec_concat, false)
  | Token.PLUS  -> Some (prec_add, false)
  | Token.MINUS -> Some (prec_add, false)
  | Token.STAR  -> Some (prec_mul, false)
  | Token.SLASH -> Some (prec_mul, false)
  | Token.IDENT "DIV" -> Some (prec_mul, false)
  | Token.IDENT "MOD" -> Some (prec_mul, false)
  | Token.POWER -> Some (prec_power, true)
  | Token.ARROW | Token.DARROW | Token.TILDE ->
    Some (prec_post, false)
  | Token.LPAREN when not (cur_ws t) -> Some (prec_post, false)
  | Token.LBRACKET when not (cur_ws t) -> Some (prec_post, false)
  | _ -> None

let tok_to_binop = function
  | Token.PLUS  -> Ast.Add  | Token.MINUS -> Ast.Sub
  | Token.STAR  -> Ast.Mul  | Token.SLASH -> Ast.Div
  | Token.POWER -> Ast.Power | Token.AMPAMP -> Ast.Concat
  | Token.EQ -> Ast.Eq | Token.NE -> Ast.Ne
  | Token.LT -> Ast.Lt | Token.GT -> Ast.Gt
  | Token.LE -> Ast.Le | Token.GE -> Ast.Ge
  | Token.IDENT "AND" -> Ast.And | Token.IDENT "OR"  -> Ast.Or
  | Token.IDENT "DIV" -> Ast.Div | Token.IDENT "MOD" -> Ast.Mod
  | Token.IDENT "EQ"  -> Ast.Eq  | Token.IDENT "NE"  -> Ast.Ne
  | Token.IDENT "LT"  -> Ast.Lt  | Token.IDENT "GT"  -> Ast.Gt
  | Token.IDENT "LE"  -> Ast.Le  | Token.IDENT "GE"  -> Ast.Ge
  | Token.IDENT "CO"  -> Ast.CO  | Token.IDENT "CN"  -> Ast.CN
  | Token.IDENT "CA"  -> Ast.CA  | Token.IDENT "NA"  -> Ast.NA
  | Token.IDENT "CS"  -> Ast.CS  | Token.IDENT "NS"  -> Ast.NS
  | Token.IDENT "CP"  -> Ast.CP  | Token.IDENT "NP"  -> Ast.NP
  | Token.IDENT "IN"  -> Ast.In
  | _ -> failwith "tok_to_binop: unreachable"

(* ---- expression parser (Pratt) ---------------------------------------- *)

let can_start_expr t =
  match cur t with
  | Token.INT _ | Token.FLOAT _ | Token.STRING _ | Token.BSTRING _
  | Token.IDENT _ | Token.FIELD_SYMBOL _
  | Token.TEMPLATE_BEGIN | Token.LPAREN
  | Token.PLUS | Token.MINUS | Token.HASH -> true
  | _ -> false

(* Forward ref so parse_constructor_args can call into the Pratt parser
   before it exists. Tied up after parse_pratt is defined. *)
let parse_expr : (t -> int -> Ast.expr) ref = ref (fun _ _ -> assert false)

(* Parse argument list between parens (caller already consumed LPAREN).
   Handles positional args and EXPORTING/IMPORTING/CHANGING/RECEIVING blocks. *)
let parse_constructor_args t =
  let args = ref [] in
  let parse_named_block t ctor stop_kws =
    while cur t <> Token.RPAREN && cur t <> Token.EOF &&
          not (List.exists (is_kw t) stop_kws) do
      let (n, _) = expect_ident t in
      ignore (expect t Token.EQ);
      let e = !parse_expr t 0 in
      args := ctor (n, e) :: !args
    done
  in
  let other_sections = ["EXPORTING";"IMPORTING";"CHANGING";"RECEIVING";"EXCEPTIONS"] in
  while cur t <> Token.RPAREN && cur t <> Token.EOF do
    match cur t with
    | Token.IDENT name when
        (match peek t 1 with Token.EQ -> true | _ -> false) &&
        not (List.mem name other_sections) ->
      advance t; advance t;
      let e = !parse_expr t 0 in
      args := Ast.Named (name, e) :: !args
    | Token.IDENT "EXPORTING" ->
      advance t;
      parse_named_block t (fun (n,e) -> Ast.NamedExporting (n,e)) other_sections
    | Token.IDENT "IMPORTING" ->
      advance t;
      parse_named_block t (fun (n,e) -> Ast.NamedImporting (n,e)) other_sections
    | Token.IDENT "CHANGING" ->
      advance t;
      parse_named_block t (fun (n,e) -> Ast.NamedChanging (n,e)) other_sections
    | Token.IDENT "RECEIVING" ->
      advance t;
      parse_named_block t (fun (n,e) -> Ast.NamedReceiving (n,e)) other_sections
    | _ ->
      let e = !parse_expr t 0 in
      args := Ast.Positional e :: !args
  done;
  List.rev !args

let parse_type_or_hash t =
  match cur t with
  | Token.HASH -> advance t; Ast.THHash
  | Token.IDENT name -> advance t; Ast.THType name
  | _ -> error t "expected type name or #"

(* VALUE body: named args interspersed with row literals ( ... ) *)
let parse_value_body t =
  let items = ref [] in
  while cur t <> Token.RPAREN && cur t <> Token.EOF do
    match cur t with
    | Token.LPAREN when cur_ws t ->
      advance t;
      let row_args = ref [] in
      while cur t <> Token.RPAREN && cur t <> Token.EOF do
        (match cur t with
         | Token.IDENT name when
             (match peek t 1 with Token.EQ -> true | _ -> false) ->
           advance t; advance t;
           let e = !parse_expr t 0 in
           row_args := Ast.Named (name, e) :: !row_args
         | _ ->
           let e = !parse_expr t 0 in
           row_args := Ast.Positional e :: !row_args)
      done;
      ignore (expect t Token.RPAREN);
      items := Ast.VBRow (List.rev !row_args) :: !items
    | Token.IDENT name when
        (match peek t 1 with Token.EQ -> true | _ -> false) ->
      advance t; advance t;
      let e = !parse_expr t 0 in
      items := Ast.VBArg (Ast.Named (name, e)) :: !items
    | Token.IDENT "LET" ->
      (* LET bindings: skip for now, consume until IN *)
      advance t;
      while cur t <> Token.RPAREN && cur t <> Token.EOF &&
            not (is_kw t "IN") do advance t done;
      if is_kw t "IN" then advance t
    | _ ->
      let e = !parse_expr t 0 in
      items := Ast.VBArg (Ast.Positional e) :: !items
  done;
  List.rev !items

(* COND/SWITCH share the WHEN...THEN...ELSE structure *)
let parse_when_branches t =
  let branches = ref [] in
  let else_expr = ref None in
  while cur t <> Token.RPAREN && cur t <> Token.EOF do
    if is_kw t "WHEN" then begin
      advance t;
      let v = !parse_expr t 0 in
      ignore (expect_kw t "THEN");
      let then_e = !parse_expr t 0 in
      branches := { Ast.cb_cond = v; cb_then = then_e } :: !branches
    end else if is_kw t "ELSE" then begin
      advance t;
      else_expr := Some (!parse_expr t 0)
    end else
      advance t
  done;
  (List.rev !branches, !else_expr)

(* Consume a constructor expression: KW type( args ).
   All eight constructors have the same shape except VALUE and SWITCH. *)
let parse_constructor t loc kw =
  advance t;
  let ty = parse_type_or_hash t in
  ignore (expect t Token.LPAREN);
  match kw with
  | "VALUE" ->
    let body = parse_value_body t in
    ignore (expect t Token.RPAREN);
    Ast.EValue (ty, body, loc)
  | "COND" ->
    let (branches, else_e) = parse_when_branches t in
    ignore (expect t Token.RPAREN);
    Ast.ECond (ty, branches, else_e, loc)
  | "SWITCH" ->
    let sw_expr = !parse_expr t 0 in
    let (branches, else_e) = parse_when_branches t in
    ignore (expect t Token.RPAREN);
    Ast.ESwitch (ty, sw_expr, branches, else_e, loc)
  | _ ->
    let args = parse_constructor_args t in
    ignore (expect t Token.RPAREN);
    (match kw with
     | "NEW"  -> Ast.ENew (ty, args, loc)
     | "CONV" -> Ast.EConv (ty, args, loc)
     | "CAST" -> Ast.ECast (ty, args, loc)
     | "REF"  -> Ast.ERef (ty, args, loc)
     | "CORRESPONDING" -> Ast.ECorresponding (ty, args, loc)
     | _ -> assert false)

let is_constructor_kw = function
  | "NEW"|"VALUE"|"CONV"|"CAST"|"COND"|"SWITCH"|"REF"|"CORRESPONDING" -> true
  | _ -> false

(* ---- Pratt: primary (nud) ---------------------------------------------- *)

let rec parse_primary t =
  let loc = cur_loc t in
  match cur t with
  | Token.INT s    -> advance t; Ast.EInt (s, loc)
  | Token.FLOAT s  -> advance t; Ast.EFloat (s, loc)
  | Token.STRING s -> advance t; Ast.EString (s, loc)
  | Token.BSTRING s -> advance t; Ast.EBString (s, loc)

  | Token.TEMPLATE_BEGIN ->
    advance t;
    let parts = ref [] in
    let done_ = ref false in
    while not !done_ do
      match cur t with
      | Token.TEMPLATE_END -> advance t; done_ := true
      | Token.TEMPLATE_TEXT s -> advance t; parts := Ast.TPText s :: !parts
      | Token.TEMPLATE_EXPR_BEGIN ->
        advance t;
        let e = !parse_expr t 0 in
        ignore (expect t Token.TEMPLATE_EXPR_END);
        parts := Ast.TPExpr e :: !parts
      | _ -> error t "unexpected token in string template"
    done;
    Ast.ETemplate (List.rev !parts, loc)

  | Token.FIELD_SYMBOL s -> advance t; Ast.EFieldSymbol (s, loc)
  | Token.HASH -> advance t; Ast.EIdent ("#", loc)
  | Token.STAR -> advance t; Ast.EStar loc

  | Token.PLUS  -> advance t; Ast.EUnOp (Ast.UPlus,  parse_pratt t 90, loc)
  | Token.MINUS -> advance t; Ast.EUnOp (Ast.UMinus, parse_pratt t 90, loc)
  | Token.IDENT "NOT" -> advance t; Ast.EUnOp (Ast.Not, parse_pratt t 30, loc)

  (* Constructor expressions: keyword followed by type name or # *)
  | Token.IDENT kw when is_constructor_kw kw &&
      (match peek t 1 with Token.IDENT _ | Token.HASH -> true | _ -> false) ->
    parse_constructor t loc kw

  | Token.LPAREN ->
    advance t;
    let e = !parse_expr t 0 in
    ignore (expect t Token.RPAREN);
    Ast.EParen (e, loc)

  | Token.IDENT s -> advance t; Ast.EIdent (s, loc)

  | _ -> error t "expected expression"

(* ---- Pratt: main loop -------------------------------------------------- *)

(* After parsing obj->member / obj=>member / obj~member, ABAP always treats
   a following LPAREN as a method call, regardless of whitespace. This is
   unlike standalone identifiers where f ( x ) is grouping, not a call. *)
and parse_member_call t left _loc =
  if cur t = Token.LPAREN then begin
    let cloc = cur_loc t in
    advance t;
    let args = parse_constructor_args t in
    ignore (expect t Token.RPAREN);
    Ast.ECall (left, args, cloc)
  end else
    left

and parse_pratt t min_prec =
  let left = ref (parse_primary t) in
  let continue_ = ref true in
  while !continue_ do
    if is_kw t "IS" then begin
      if prec_cmp >= min_prec then begin
        let loc = cur_loc t in
        advance t;
        let negated = is_kw t "NOT" in
        if negated then advance t;
        let test =
          if is_kw t "INITIAL" then
            (advance t; if negated then Ast.IsNotInitial else Ast.IsInitial)
          else if is_kw t "BOUND" then
            (advance t; if negated then Ast.IsNotBound else Ast.IsBound)
          else if is_kw t "ASSIGNED" then
            (advance t; if negated then Ast.IsNotAssigned else Ast.IsAssigned)
          else if is_kw t "SUPPLIED" then
            (advance t; Ast.IsSupplied)
          else if is_kw t "INSTANCE" then begin
            advance t; ignore (expect_kw t "OF");
            let (cls, _) = expect_ident t in Ast.IsInstance cls
          end else
            error t "expected IS test (INITIAL/BOUND/ASSIGNED/SUPPLIED/INSTANCE)"
        in
        left := Ast.EIs (!left, test, loc)
      end else
        continue_ := false
    end
    else if is_kw t "BETWEEN" then begin
      if prec_cmp >= min_prec then begin
        let loc = cur_loc t in
        advance t;
        let low = !parse_expr t (prec_cmp + 1) in
        ignore (expect_kw t "AND");
        let high = !parse_expr t (prec_cmp + 1) in
        left := Ast.EBinOp (Ast.Between, !left,
          Ast.EBinOp (Ast.And, low, high, loc), loc)
      end else
        continue_ := false
    end
    else
      match infix_info t with
      | None -> continue_ := false
      | Some (prec, right_assoc) ->
        if prec >= min_prec then begin
          let loc = cur_loc t in
          match cur t with
          | Token.ARROW ->
            advance t;
            let (member, _) = expect_ident t in
            left := parse_member_call t (Ast.EArrow (!left, member, loc)) loc
          | Token.DARROW ->
            advance t;
            let (member, _) = expect_ident t in
            left := parse_member_call t (Ast.EDArrow (!left, member, loc)) loc
          | Token.TILDE ->
            advance t;
            let (member, _) = expect_ident t in
            left := parse_member_call t (Ast.ETilde (!left, member, loc)) loc
          | Token.LPAREN when not (cur_ws t) ->
            advance t;
            let args = parse_constructor_args t in
            ignore (expect t Token.RPAREN);
            left := Ast.ECall (!left, args, loc)
          | Token.LBRACKET when not (cur_ws t) ->
            advance t;
            let keys = ref [] in
            while cur t <> Token.RBRACKET && cur t <> Token.EOF do
              keys := !parse_expr t 0 :: !keys
            done;
            ignore (expect t Token.RBRACKET);
            left := Ast.ETableExpr (!left, List.rev !keys, loc)
          | tok ->
            let op = tok_to_binop tok in
            advance t;
            let next_prec = if right_assoc then prec else prec + 1 in
            left := Ast.EBinOp (op, !left, parse_pratt t next_prec, loc)
        end else
          continue_ := false
  done;
  !left

let () = parse_expr := (fun t min_prec -> parse_pratt t min_prec)

let parse_expr_top t = parse_pratt t 0

(* ---- type spec --------------------------------------------------------- *)

let parse_type_spec t =
  if is_kw t "TYPE" then begin
    advance t;
    if is_kw t "REF" then begin
      advance t; ignore (expect_kw t "TO");
      let (name, _) = expect_ident t in
      Ast.TRef name
    end else if is_kw t "TABLE" then begin
      advance t; ignore (expect_kw t "OF");
      let inner = if is_kw t "REF" then begin
        advance t; ignore (expect_kw t "TO");
        let (name, _) = expect_ident t in Ast.TRef name
      end else
        let (name, _) = expect_ident t in Ast.TSimple name
      in
      if is_kw t "WITH" then
        skip_to t [Token.DOT; Token.COMMA; Token.RPAREN];
      Ast.TTable (Ast.Standard, inner)
    end else if is_kw t "SORTED" || is_kw t "HASHED" || is_kw t "STANDARD" then begin
      let kind = match cur_ident t with
        | "SORTED" -> Ast.Sorted | "HASHED" -> Ast.Hashed | _ -> Ast.Standard
      in
      advance t;
      ignore (expect_kw t "TABLE"); ignore (expect_kw t "OF");
      let (name, _) = expect_ident t in
      if is_kw t "WITH" then
        skip_to t [Token.DOT; Token.COMMA; Token.RPAREN];
      Ast.TTable (kind, Ast.TSimple name)
    end else begin
      let (name, _) = expect_ident t in
      Ast.TSimple name
    end
  end else if is_kw t "LIKE" then begin
    advance t;
    if is_kw t "TABLE" then begin
      advance t; ignore (expect_kw t "OF");
      let (name, _) = expect_ident t in
      Ast.TTable (Ast.Standard, Ast.TSimple name)
    end else
      let (name, _) = expect_ident t in Ast.TLike name
  end else
    error t "expected TYPE or LIKE"

(* ---- declarations ------------------------------------------------------ *)

let parse_one_decl t =
  let loc = cur_loc t in
  if is_kw t "BEGIN" then begin
    advance t; ignore (expect_kw t "OF");
    let (sname, _) = expect_ident t in
    skip_to t [Token.DOT; Token.COMMA];
    if cur t = Token.DOT then advance t
    else if cur t = Token.COMMA then advance t;
    let fields = ref [] in
    while not (is_kw t "END") && cur t <> Token.EOF do
      let floc = cur_loc t in
      let (fname, _) = expect_ident t in
      let fty =
        if is_kw t "TYPE" || is_kw t "LIKE" then parse_type_spec t
        else Ast.TSimple "ANY"
      in
      skip_to t [Token.DOT; Token.COMMA];
      if cur t = Token.DOT then advance t
      else if cur t = Token.COMMA then advance t;
      fields := { Ast.f_name = fname; f_type = fty; f_loc = floc } :: !fields
    done;
    ignore (expect_kw t "END"); ignore (expect_kw t "OF"); ignore (expect_ident t);
    { Ast.d_name = sname; d_type = Some (Ast.TStruct (List.rev !fields));
      d_value = None; d_loc = loc }
  end else match cur t with
  | Token.IDENT name ->
    advance t;
    let ty =
      if is_kw t "TYPE" || is_kw t "LIKE" then Some (parse_type_spec t)
      else None
    in
    let value =
      if is_kw t "VALUE" then (advance t; Some (parse_expr_top t))
      else None
    in
    skip_to t [Token.DOT; Token.COMMA];
    { Ast.d_name = name; d_type = ty; d_value = value; d_loc = loc }
  | _ -> error t "expected declaration name"

let parse_one_fs_decl t =
  let loc = cur_loc t in
  match cur t with
  | Token.FIELD_SYMBOL name ->
    advance t;
    let ty =
      if is_kw t "TYPE" || is_kw t "LIKE" then Some (parse_type_spec t)
      else None
    in
    skip_to t [Token.DOT; Token.COMMA];
    { Ast.d_name = name; d_type = ty; d_value = None; d_loc = loc }
  | _ -> error t "expected field symbol <name>"

(* Chained or single: DATA x TYPE t.  /  DATA: x, y, z. *)
let parse_decl_stmt t parse_one =
  let loc = cur_loc t in
  if cur t = Token.COLON then begin
    advance t;
    let decls = ref [parse_one t] in
    while cur t = Token.COMMA do
      advance t; decls := parse_one t :: !decls
    done;
    (List.rev !decls, loc)
  end else
    ([parse_one t], loc)

(* ---- statement parsers ------------------------------------------------- *)

let collect_meta t =
  let stmts = ref [] in
  let continue_ = ref true in
  while !continue_ do
    match cur t with
    | Token.COMMENT s ->
      let loc = cur_loc t in advance t;
      stmts := Ast.SComment (s, loc) :: !stmts
    | Token.PRAGMA s ->
      let loc = cur_loc t in advance t;
      if cur t = Token.DOT then advance t;
      stmts := Ast.SPragma (s, loc) :: !stmts
    | _ -> continue_ := false
  done;
  List.rev !stmts

let rec parse_body t terminators =
  let stmts = ref [] in
  let done_ = ref false in
  while not !done_ && cur t <> Token.EOF do
    let meta = collect_meta t in
    stmts := List.rev_append meta !stmts;
    match cur t with
    | Token.IDENT kw when List.exists (String.equal kw) terminators ->
      done_ := true
    | Token.EOF -> done_ := true
    | _ -> stmts := parse_stmt t :: !stmts
  done;
  List.rev !stmts

and parse_stmt t =
  let meta = collect_meta t in
  match meta with
  | (s :: _) -> s
  | [] ->
  let loc = cur_loc t in
  match cur t with
  | Token.EOF -> error t "unexpected end of file"

  | Token.IDENT "REPORT" ->
    advance t;
    let (name, _) = expect_ident t in
    skip_to t [Token.DOT]; expect_dot t;
    Ast.SReport (name, loc)

  | Token.IDENT "PROGRAM" ->
    advance t;
    let name =
      if cur t <> Token.DOT then let (n, _) = expect_ident t in n
      else ""
    in
    skip_to t [Token.DOT]; expect_dot t;
    Ast.SProgram (name, loc)

  | Token.IDENT "INCLUDE" ->
    advance t;
    let (name, _) = expect_ident t in
    skip_to t [Token.DOT]; expect_dot t;
    Ast.SInclude (name, loc)

  | Token.IDENT "DATA" ->
    advance t;
    if cur t = Token.LPAREN && not (cur_ws t) then begin
      (* Inline declaration: DATA(x) = expr *)
      advance t;
      let (name, _) = expect_ident t in
      ignore (expect t Token.RPAREN);
      ignore (expect t Token.EQ);
      let rhs = parse_expr_top t in
      expect_dot t;
      Ast.SInlineDecl (name, rhs, loc)
    end else begin
      let (decls, dloc) = parse_decl_stmt t parse_one_decl in
      expect_dot t; Ast.SData (decls, dloc)
    end

  | Token.IDENT "CONSTANTS" ->
    advance t;
    let (decls, dloc) = parse_decl_stmt t parse_one_decl in
    expect_dot t; Ast.SConstants (decls, dloc)

  | Token.IDENT "TYPES" ->
    advance t;
    let (decls, dloc) = parse_decl_stmt t parse_one_decl in
    expect_dot t; Ast.STypes (decls, dloc)

  | Token.IDENT "STATICS" ->
    advance t;
    let (decls, dloc) = parse_decl_stmt t parse_one_decl in
    expect_dot t; Ast.SStatics (decls, dloc)

  | Token.IDENT "FIELD-SYMBOLS" ->
    advance t;
    let (decls, dloc) = parse_decl_stmt t parse_one_fs_decl in
    expect_dot t; Ast.SFieldSymbols (decls, dloc)

  | Token.IDENT "IF" ->
    advance t;
    let cond = parse_expr_top t in
    expect_dot t;
    let then_body = parse_body t ["ELSEIF"; "ELSE"; "ENDIF"] in
    let elseifs = ref [] in
    while is_kw t "ELSEIF" do
      advance t;
      let econd = parse_expr_top t in
      expect_dot t;
      let ebody = parse_body t ["ELSEIF"; "ELSE"; "ENDIF"] in
      elseifs := (econd, ebody) :: !elseifs
    done;
    let else_body =
      if is_kw t "ELSE" then
        (advance t; expect_dot t; Some (parse_body t ["ENDIF"]))
      else None
    in
    ignore (expect_kw t "ENDIF"); expect_dot t;
    Ast.SIf ({ Ast.if_cond = cond; if_then = then_body;
               if_elseifs = List.rev !elseifs; if_else = else_body }, loc)

  | Token.IDENT "CASE" ->
    advance t;
    let e = parse_expr_top t in
    expect_dot t;
    let whens = ref [] in
    let others = ref None in
    let done_ = ref false in
    while not !done_ do
      ignore (collect_meta t);
      if is_kw t "WHEN" then begin
        advance t;
        if is_kw t "OTHERS" then begin
          advance t; expect_dot t;
          others := Some (parse_body t ["WHEN"; "ENDCASE"])
        end else begin
          let vals = ref [parse_expr_top t] in
          while is_kw t "OR" do advance t; vals := parse_expr_top t :: !vals done;
          expect_dot t;
          let body = parse_body t ["WHEN"; "ENDCASE"] in
          whens := { Ast.cw_values = List.rev !vals; cw_body = body } :: !whens
        end
      end else
        done_ := true
    done;
    ignore (expect_kw t "ENDCASE"); expect_dot t;
    Ast.SCase (e, List.rev !whens, !others, loc)

  | Token.IDENT "DO" ->
    advance t;
    let times =
      if can_start_expr t && not (is_kw t "VARYING") then
        let e = parse_expr_top t in
        ignore (expect_kw t "TIMES"); Some e
      else None
    in
    skip_to t [Token.DOT]; expect_dot t;
    let body = parse_body t ["ENDDO"] in
    ignore (expect_kw t "ENDDO"); expect_dot t;
    Ast.SDo (times, body, loc)

  | Token.IDENT "WHILE" ->
    advance t;
    let cond = parse_expr_top t in
    expect_dot t;
    let body = parse_body t ["ENDWHILE"] in
    ignore (expect_kw t "ENDWHILE"); expect_dot t;
    Ast.SWhile (cond, body, loc)

  | Token.IDENT "LOOP" ->
    advance t;
    let spec =
      if is_kw t "AT" then begin
        advance t;
        let e = parse_expr_top t in
        skip_to t [Token.DOT];
        Ast.LoopAt e
      end else begin
        skip_to t [Token.DOT];
        Ast.LoopGeneric
      end
    in
    expect_dot t;
    let body = parse_body t ["ENDLOOP"] in
    ignore (expect_kw t "ENDLOOP"); expect_dot t;
    Ast.SLoop (spec, body, loc)

  | Token.IDENT "CHECK" ->
    advance t; let e = parse_expr_top t in expect_dot t; Ast.SCheck (e, loc)
  | Token.IDENT "CONTINUE" -> advance t; expect_dot t; Ast.SContinue loc
  | Token.IDENT "EXIT"     -> advance t; expect_dot t; Ast.SExit loc
  | Token.IDENT "RETURN"   -> advance t; expect_dot t; Ast.SReturn loc
  | Token.IDENT "STOP"     -> advance t; expect_dot t; Ast.SStop loc

  | Token.IDENT "WRITE" ->
    advance t;
    let items = ref [] in
    let parse_write_items () =
      while cur t <> Token.DOT && cur t <> Token.COMMA && cur t <> Token.EOF do
        match cur t with
        | Token.SLASH -> advance t; items := Ast.WSlash :: !items
        | _ -> items := Ast.WExpr (parse_expr_top t) :: !items
      done
    in
    if cur t = Token.COLON then begin
      advance t;
      parse_write_items ();
      while cur t = Token.COMMA do advance t; parse_write_items () done
    end else
      parse_write_items ();
    expect_dot t;
    Ast.SWrite (List.rev !items, loc)

  | Token.IDENT "CLEAR" ->
    advance t;
    let e = parse_expr_top t in
    skip_to t [Token.DOT]; expect_dot t;
    Ast.SClear (e, loc)

  | Token.IDENT "FREE" ->
    advance t;
    let e = parse_expr_top t in
    skip_to t [Token.DOT]; expect_dot t;
    Ast.SFree (e, loc)

  | Token.IDENT "ASSIGN" ->
    advance t;
    let src = parse_expr_top t in
    ignore (expect_kw t "TO");
    let fs = parse_expr_top t in
    skip_to t [Token.DOT]; expect_dot t;
    Ast.SAssignTo (src, fs, loc)

  | Token.IDENT "APPEND" ->
    advance t;
    let e = parse_expr_top t in
    let target =
      if is_kw t "TO" then (advance t; Some (parse_expr_top t))
      else None
    in
    skip_to t [Token.DOT]; expect_dot t;
    Ast.SAppend (e, target, loc)

  | Token.IDENT "INSERT" ->
    advance t;
    let e = parse_expr_top t in
    let target =
      if is_kw t "INTO" then begin
        advance t;
        if is_kw t "TABLE" then advance t;
        Some (parse_expr_top t)
      end else None
    in
    skip_to t [Token.DOT]; expect_dot t;
    Ast.SInsert (e, target, loc)

  | Token.IDENT "DELETE" ->
    advance t; let e = parse_expr_top t in
    skip_to t [Token.DOT]; expect_dot t; Ast.SDelete (e, loc)

  | Token.IDENT "SORT" ->
    advance t; let e = parse_expr_top t in
    skip_to t [Token.DOT]; expect_dot t; Ast.SSort (e, loc)

  | Token.IDENT "READ" ->
    advance t; skip_to t [Token.DOT]; expect_dot t; Ast.SRead loc

  | Token.IDENT "MODIFY" ->
    advance t; skip_to t [Token.DOT]; expect_dot t; Ast.SModify loc

  | Token.IDENT "MOVE" ->
    advance t;
    if is_kw t "CORRESPONDING" || is_kw t "MOVE-CORRESPONDING" then begin
      advance t;
      let src = parse_expr_top t in
      ignore (expect_kw t "TO");
      let dst = parse_expr_top t in
      expect_dot t; Ast.SMoveCorr (src, dst, loc)
    end else begin
      let src = parse_expr_top t in
      ignore (expect_kw t "TO");
      let dst = parse_expr_top t in
      expect_dot t; Ast.SMove (src, dst, loc)
    end

  | Token.IDENT "MOVE-CORRESPONDING" ->
    advance t;
    let src = parse_expr_top t in
    ignore (expect_kw t "TO");
    let dst = parse_expr_top t in
    expect_dot t; Ast.SMoveCorr (src, dst, loc)

  | Token.IDENT "FORM" ->
    advance t;
    let (name, _) = expect_ident t in
    let using = ref [] in
    let changing = ref [] in
    if is_kw t "USING" then begin
      advance t;
      while cur t <> Token.DOT && not (is_kw t "CHANGING") && cur t <> Token.EOF do
        let (p, _) = expect_ident t in
        if is_kw t "TYPE" || is_kw t "LIKE" then
          (advance t; ignore (expect_ident t));
        using := p :: !using
      done
    end;
    if is_kw t "CHANGING" then begin
      advance t;
      while cur t <> Token.DOT && cur t <> Token.EOF do
        let (p, _) = expect_ident t in
        if is_kw t "TYPE" || is_kw t "LIKE" then
          (advance t; ignore (expect_ident t));
        changing := p :: !changing
      done
    end;
    expect_dot t;
    let body = parse_body t ["ENDFORM"] in
    ignore (expect_kw t "ENDFORM"); expect_dot t;
    Ast.SForm (name, List.rev !using, List.rev !changing, body, loc)

  | Token.IDENT "PERFORM" ->
    advance t;
    let (name, _) = expect_ident t in
    let args = ref [] in
    while cur t <> Token.DOT && cur t <> Token.EOF do
      if is_kw t "USING" || is_kw t "CHANGING" then advance t
      else args := parse_expr_top t :: !args
    done;
    expect_dot t;
    Ast.SPerform (name, List.rev !args, loc)

  | Token.IDENT "CLASS" when is_kw_tok (peek t 2) "DEFINITION" ->
    advance t;
    let (name, _) = expect_ident t in
    ignore (expect_kw t "DEFINITION");
    let super =
      if is_kw t "INHERITING" then begin
        advance t; ignore (expect_kw t "FROM");
        let (s, _) = expect_ident t in Some s
      end else None
    in
    skip_to t [Token.DOT]; expect_dot t;
    let pub = ref [] and priv = ref [] and prot = ref [] in
    let current = ref pub in
    let done_ = ref false in
    while not !done_ do
      ignore (collect_meta t);
      if is_kw t "PUBLIC" then
        (advance t; ignore (expect_kw t "SECTION"); expect_dot t; current := pub)
      else if is_kw t "PRIVATE" then
        (advance t; ignore (expect_kw t "SECTION"); expect_dot t; current := priv)
      else if is_kw t "PROTECTED" then
        (advance t; ignore (expect_kw t "SECTION"); expect_dot t; current := prot)
      else if is_kw t "ENDCLASS" then
        done_ := true
      else if cur t = Token.EOF then
        done_ := true
      else
        !current := parse_stmt t :: !(!current)
    done;
    ignore (expect_kw t "ENDCLASS"); expect_dot t;
    Ast.SClassDef ({
      Ast.cls_name = name; cls_super = super;
      cls_public = List.rev !pub;
      cls_private = List.rev !priv;
      cls_protected = List.rev !prot }, loc)

  | Token.IDENT "CLASS" when is_kw_tok (peek t 2) "IMPLEMENTATION" ->
    advance t;
    let (name, _) = expect_ident t in
    ignore (expect_kw t "IMPLEMENTATION");
    skip_to t [Token.DOT]; expect_dot t;
    let body = parse_body t ["ENDCLASS"] in
    ignore (expect_kw t "ENDCLASS"); expect_dot t;
    Ast.SClassImpl (name, body, loc)

  | Token.IDENT "METHOD" ->
    advance t;
    let (name, _) = expect_ident t in
    skip_to t [Token.DOT]; expect_dot t;
    let body = parse_body t ["ENDMETHOD"] in
    ignore (expect_kw t "ENDMETHOD"); expect_dot t;
    Ast.SMethod (name, body, loc)

  | Token.IDENT "INTERFACE" ->
    advance t;
    let (name, _) = expect_ident t in
    skip_to t [Token.DOT]; expect_dot t;
    let body = parse_body t ["ENDINTERFACE"] in
    ignore (expect_kw t "ENDINTERFACE"); expect_dot t;
    Ast.SInterface (name, body, loc)

  | Token.IDENT "TRY" ->
    advance t; expect_dot t;
    let body = parse_body t ["CATCH"; "CLEANUP"; "ENDTRY"] in
    let catches = ref [] in
    while is_kw t "CATCH" do
      advance t;
      let types = ref [] in
      while (match cur t with Token.IDENT _ -> true | _ -> false) &&
            not (is_kw t "INTO") do
        let (ty, _) = expect_ident t in types := ty :: !types
      done;
      let into =
        if is_kw t "INTO" then
          (advance t; let (v, _) = expect_ident t in Some v)
        else None
      in
      expect_dot t;
      let cbody = parse_body t ["CATCH"; "CLEANUP"; "ENDTRY"] in
      catches := { Ast.catch_types = List.rev !types;
                   catch_into = into; catch_body = cbody } :: !catches
    done;
    let cleanup =
      if is_kw t "CLEANUP" then
        (advance t; expect_dot t; Some (parse_body t ["ENDTRY"]))
      else None
    in
    ignore (expect_kw t "ENDTRY"); expect_dot t;
    Ast.STry (body, List.rev !catches, cleanup, loc)

  | Token.IDENT "RAISE" ->
    advance t;
    let e = parse_expr_top t in
    skip_to t [Token.DOT]; expect_dot t;
    Ast.SRaise (e, loc)

  (* SELECT is an eldritch horror of SQL embedded in ABAP. We don't parse
     the contents, just find matching ENDSELECT or bail at the first DOT
     for single-row selects. Good enough to not derail everything else. *)
  | Token.IDENT "SELECT" ->
    advance t;
    let depth = ref 1 in
    while !depth > 0 && cur t <> Token.EOF do
      if is_kw t "SELECT" then incr depth;
      if is_kw t "ENDSELECT" then begin
        decr depth;
        if !depth > 0 then advance t
      end else if cur t = Token.DOT && !depth = 1 then
        (advance t; depth := 0)
      else
        advance t
    done;
    if is_kw t "ENDSELECT" then (advance t; expect_dot t);
    Ast.SSelect loc

  | Token.IDENT "MESSAGE" ->
    advance t; skip_to t [Token.DOT]; expect_dot t; Ast.SMessage loc

  | Token.IDENT "CREATE" when is_kw_tok (peek t 1) "OBJECT" ->
    advance t; advance t;
    let target = parse_expr_top t in
    let args =
      if cur t <> Token.DOT then begin
        if is_kw t "EXPORTING" then advance t;
        let a = ref [] in
        while cur t <> Token.DOT && cur t <> Token.EOF do
          match cur t with
          | Token.IDENT name when
              (match peek t 1 with Token.EQ -> true | _ -> false) ->
            advance t; advance t;
            a := Ast.Named (name, parse_expr_top t) :: !a
          | Token.IDENT "TYPE" ->
            while cur t <> Token.DOT && cur t <> Token.EOF &&
                  not (match cur t with
                       | Token.IDENT n ->
                         (match peek t 1 with Token.EQ -> true | _ -> false) &&
                         not (String.equal n "TYPE")
                       | _ -> false) do
              advance t
            done
          | _ -> advance t
        done;
        List.rev !a
      end else []
    in
    expect_dot t;
    Ast.SCreateObject (target, args, loc)

  | Token.IDENT "CREATE" when is_kw_tok (peek t 1) "DATA" ->
    advance t; advance t;
    let target = parse_expr_top t in
    skip_to t [Token.DOT]; expect_dot t;
    Ast.SCreateData (target, loc)

  | Token.IDENT "CALL" when is_kw_tok (peek t 1) "FUNCTION" ->
    advance t; advance t;
    let fname = match cur t with
      | Token.STRING s | Token.BSTRING s -> advance t; s
      | Token.IDENT s -> advance t; s
      | _ -> error t "expected function name"
    in
    let args = ref [] in
    let section_kws = ["EXPORTING";"IMPORTING";"CHANGING";"RECEIVING";"EXCEPTIONS";"TABLES"] in
    while cur t <> Token.DOT && cur t <> Token.EOF do
      if is_kw t "EXPORTING" then begin
        advance t;
        while cur t <> Token.DOT && cur t <> Token.EOF &&
              not (List.exists (is_kw t) section_kws) do
          let (n, _) = expect_ident t in
          ignore (expect t Token.EQ);
          args := Ast.NamedExporting (n, parse_expr_top t) :: !args
        done
      end else if is_kw t "IMPORTING" then begin
        advance t;
        while cur t <> Token.DOT && cur t <> Token.EOF &&
              not (List.exists (is_kw t) section_kws) do
          let (n, _) = expect_ident t in
          ignore (expect t Token.EQ);
          args := Ast.NamedImporting (n, parse_expr_top t) :: !args
        done
      end else if is_kw t "CHANGING" then begin
        advance t;
        while cur t <> Token.DOT && cur t <> Token.EOF &&
              not (List.exists (is_kw t) section_kws) do
          let (n, _) = expect_ident t in
          ignore (expect t Token.EQ);
          args := Ast.NamedChanging (n, parse_expr_top t) :: !args
        done
      end else if is_kw t "EXCEPTIONS" then begin
        advance t; skip_to t [Token.DOT]
      end else
        advance t
    done;
    expect_dot t;
    Ast.SCallFunction (fname, List.rev !args, loc)

  | Token.IDENT "CALL" when is_kw_tok (peek t 1) "METHOD" ->
    advance t; advance t;
    let target = parse_expr_top t in
    let args =
      if cur t <> Token.DOT then begin
        if is_kw t "EXPORTING" then advance t;
        let a = ref [] in
        while cur t <> Token.DOT && cur t <> Token.EOF do
          match cur t with
          | Token.IDENT name when
              (match peek t 1 with Token.EQ -> true | _ -> false) ->
            advance t; advance t;
            a := Ast.Named (name, parse_expr_top t) :: !a
          | _ -> advance t
        done;
        List.rev !a
      end else []
    in
    expect_dot t;
    Ast.SCallMethod (target, args, loc)

  (* Declaration stubs we recognise but don't fully parse yet *)
  | Token.IDENT ("METHODS" | "CLASS-METHODS" | "CLASS-DATA" | "INTERFACES"
                 | "EVENTS" | "ALIASES") ->
    let kw = cur_ident t in
    advance t;
    let toks = ref [] in
    while cur t <> Token.DOT && cur t <> Token.EOF do
      toks := (cur t, cur_loc t) :: !toks; advance t
    done;
    expect_dot t;
    Ast.SUnknown (kw, List.rev !toks, loc)

  | Token.PRAGMA s ->
    advance t;
    if cur t = Token.DOT then advance t;
    Ast.SPragma (s, loc)

  | Token.IDENT _ -> parse_ident_stmt t loc

  | Token.FIELD_SYMBOL _ ->
    let lhs = parse_expr_top t in
    if cur t = Token.EQ then begin
      advance t;
      let rhs = parse_expr_top t in
      expect_dot t; Ast.SAssign (lhs, rhs, loc)
    end else if cur t = Token.CAST_EQ then begin
      advance t;
      let rhs = parse_expr_top t in
      expect_dot t; Ast.SCastAssign (lhs, rhs, loc)
    end else begin
      expect_dot t; Ast.SExprStmt (lhs, loc)
    end

  | _ ->
    let toks = ref [] in
    while cur t <> Token.DOT && cur t <> Token.EOF do
      toks := (cur t, cur_loc t) :: !toks; advance t
    done;
    if cur t = Token.DOT then advance t;
    Ast.SUnknown ("?", List.rev !toks, loc)

(* Identifier-started statement: could be assignment, expression, or
   something we've never heard of. Parse LHS above comparison precedence
   so = stays available as assignment rather than being swallowed as
   the equality operator. A small act of diplomacy in a lawless grammar. *)
and parse_ident_stmt t loc =
  let saved = t.pos in
  let lhs =
    try parse_pratt t (prec_cmp + 1)
    with _ -> t.pos <- saved; Ast.EIdent (cur_ident t, loc)
  in
  match cur t with
  | Token.EQ ->
    advance t;
    let rhs = parse_expr_top t in
    expect_dot t; Ast.SAssign (lhs, rhs, loc)
  | Token.CAST_EQ ->
    advance t;
    let rhs = parse_expr_top t in
    expect_dot t; Ast.SCastAssign (lhs, rhs, loc)
  | Token.DOT ->
    advance t; Ast.SExprStmt (lhs, loc)
  | _ ->
    (* Might have stopped at a comparison op — retry as full expression *)
    t.pos <- saved;
    let lhs2 =
      try parse_expr_top t
      with _ -> t.pos <- saved; Ast.EIdent (cur_ident t, loc)
    in
    if cur t = Token.DOT then
      (advance t; Ast.SExprStmt (lhs2, loc))
    else begin
      t.pos <- saved;
      let kw = cur_ident t in
      advance t;
      let toks = ref [] in
      while cur t <> Token.DOT && cur t <> Token.EOF do
        toks := (cur t, cur_loc t) :: !toks; advance t
      done;
      if cur t = Token.DOT then advance t;
      Ast.SUnknown (kw, List.rev !toks, loc)
    end

(* ---- entry point ------------------------------------------------------- *)

let parse_program tokens =
  let t = create tokens in
  let stmts = ref [] in
  while cur t <> Token.EOF do
    let meta = collect_meta t in
    stmts := List.rev_append meta !stmts;
    if cur t <> Token.EOF then
      stmts := parse_stmt t :: !stmts
  done;
  { Ast.p_stmts = List.rev !stmts }
