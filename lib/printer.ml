(* S-expression AST printer. Mostly for staring at in disbelief. *)

open Ast

let pp = Format.fprintf

let pp_binop fmt = function
  | Add -> pp fmt "+" | Sub -> pp fmt "-" | Mul -> pp fmt "*"
  | Div -> pp fmt "/" | Mod -> pp fmt "MOD" | Power -> pp fmt "**"
  | Concat -> pp fmt "&&" | And -> pp fmt "AND" | Or -> pp fmt "OR"
  | Eq -> pp fmt "=" | Ne -> pp fmt "<>" | Lt -> pp fmt "<"
  | Gt -> pp fmt ">" | Le -> pp fmt "<=" | Ge -> pp fmt ">="
  | CO -> pp fmt "CO" | CN -> pp fmt "CN" | CA -> pp fmt "CA"
  | NA -> pp fmt "NA" | CS -> pp fmt "CS" | NS -> pp fmt "NS"
  | CP -> pp fmt "CP" | NP -> pp fmt "NP"
  | In -> pp fmt "IN" | Between -> pp fmt "BETWEEN"

let pp_unop fmt = function
  | UPlus -> pp fmt "+" | UMinus -> pp fmt "-" | Not -> pp fmt "NOT"

let pp_toh fmt = function
  | THHash -> pp fmt "#" | THType s -> pp fmt "%s" s

let rec pp_expr fmt = function
  | EInt (s, _) -> pp fmt "%s" s
  | EFloat (s, _) -> pp fmt "%s" s
  | EString (s, _) -> pp fmt "'%s'" s
  | EBString (s, _) -> pp fmt "`%s`" s
  | ETemplate (parts, _) ->
    pp fmt "(template";
    List.iter (function
      | TPText s -> pp fmt " \"%s\"" s
      | TPExpr e -> pp fmt " {%a}" pp_expr e) parts;
    pp fmt ")"
  | EIdent (s, _) -> pp fmt "%s" s
  | EFieldSymbol (s, _) -> pp fmt "<%s>" s
  | EStar _ -> pp fmt "*"
  | EBinOp (op, l, r, _) ->
    pp fmt "(%a %a %a)" pp_binop op pp_expr l pp_expr r
  | EUnOp (op, e, _) ->
    pp fmt "(%a %a)" pp_unop op pp_expr e
  | EArrow (obj, mem, _) -> pp fmt "%a->%s" pp_expr obj mem
  | EDArrow (obj, mem, _) -> pp fmt "%a=>%s" pp_expr obj mem
  | ETilde (obj, mem, _) -> pp fmt "%a~%s" pp_expr obj mem
  | ECall (callee, args, _) ->
    pp fmt "(call %a" pp_expr callee;
    List.iter (fun a -> pp fmt " %a" pp_named_arg a) args;
    pp fmt ")"
  | ETableExpr (tbl, keys, _) ->
    pp fmt "%a[" pp_expr tbl;
    List.iter (fun k -> pp fmt " %a" pp_expr k) keys;
    pp fmt "]"
  | ENew (ty, args, _) -> pp_ctor fmt "NEW" ty args
  | EConv (ty, args, _) -> pp_ctor fmt "CONV" ty args
  | ECast (ty, args, _) -> pp_ctor fmt "CAST" ty args
  | ERef (ty, args, _) -> pp_ctor fmt "REF" ty args
  | ECorresponding (ty, args, _) -> pp_ctor fmt "CORRESPONDING" ty args
  | EValue (ty, body, _) ->
    pp fmt "(VALUE %a" pp_toh ty;
    List.iter (function
      | VBArg a -> pp fmt " %a" pp_named_arg a
      | VBRow args ->
        pp fmt " (";
        List.iter (fun a -> pp fmt " %a" pp_named_arg a) args;
        pp fmt " )") body;
    pp fmt ")"
  | ECond (ty, branches, else_, _) ->
    pp fmt "(COND %a" pp_toh ty;
    List.iter (fun b ->
      pp fmt " (WHEN %a THEN %a)" pp_expr b.cb_cond pp_expr b.cb_then) branches;
    (match else_ with Some e -> pp fmt " (ELSE %a)" pp_expr e | None -> ());
    pp fmt ")"
  | ESwitch (ty, sw, branches, else_, _) ->
    pp fmt "(SWITCH %a %a" pp_toh ty pp_expr sw;
    List.iter (fun b ->
      pp fmt " (WHEN %a THEN %a)" pp_expr b.cb_cond pp_expr b.cb_then) branches;
    (match else_ with Some e -> pp fmt " (ELSE %a)" pp_expr e | None -> ());
    pp fmt ")"
  | EIs (e, test, _) ->
    pp fmt "(%a IS %a)" pp_expr e pp_is_test test
  | EParen (e, _) ->
    pp fmt "(%a)" pp_expr e

and pp_ctor fmt kw ty args =
  pp fmt "(%s %a" kw pp_toh ty;
  List.iter (fun a -> pp fmt " %a" pp_named_arg a) args;
  pp fmt ")"

and pp_named_arg fmt = function
  | Positional e -> pp_expr fmt e
  | Named (n, e) -> pp fmt "%s = %a" n pp_expr e
  | NamedExporting (n, e) -> pp fmt "EXPORTING %s = %a" n pp_expr e
  | NamedImporting (n, e) -> pp fmt "IMPORTING %s = %a" n pp_expr e
  | NamedChanging (n, e) -> pp fmt "CHANGING %s = %a" n pp_expr e
  | NamedReceiving (n, e) -> pp fmt "RECEIVING %s = %a" n pp_expr e

and pp_is_test fmt = function
  | IsInitial -> pp fmt "INITIAL" | IsBound -> pp fmt "BOUND"
  | IsAssigned -> pp fmt "ASSIGNED" | IsSupplied -> pp fmt "SUPPLIED"
  | IsInstance s -> pp fmt "INSTANCE OF %s" s
  | IsNotInitial -> pp fmt "NOT INITIAL" | IsNotBound -> pp fmt "NOT BOUND"
  | IsNotAssigned -> pp fmt "NOT ASSIGNED"

let rec pp_type_spec fmt = function
  | TSimple s -> pp fmt "(type %s)" s
  | TRef s -> pp fmt "(ref %s)" s
  | TLike s -> pp fmt "(like %s)" s
  | TTable (_, inner) -> pp fmt "(table %a)" pp_type_spec inner
  | TStruct fields ->
    pp fmt "(struct";
    List.iter (fun f -> pp fmt " (%s %a)" f.f_name pp_type_spec f.f_type) fields;
    pp fmt ")"

let pp_opt_type fmt = function
  | Some ty -> pp fmt " : %a" pp_type_spec ty | None -> ()

let pp_opt_value fmt = function
  | Some v -> pp fmt " (value %a)" pp_expr v | None -> ()

let rec pp_body fmt body =
  List.iter (fun s -> pp fmt "%a@\n" pp_stmt s) body

and pp_stmt fmt = function
  | SComment (s, _) -> pp fmt "(* %s)" s
  | SPragma (s, _) -> pp fmt "(##%s)" s
  | SReport (s, _) -> pp fmt "(report %s)" s
  | SProgram (s, _) -> pp fmt "(program %s)" s
  | SInclude (s, _) -> pp fmt "(include %s)" s

  | SData (decls, _) ->
    List.iter (fun d ->
      pp fmt "(data %s%a%a)@\n" d.d_name pp_opt_type d.d_type pp_opt_value d.d_value
    ) decls
  | SConstants (decls, _) ->
    List.iter (fun d ->
      pp fmt "(constants %s%a%a)@\n" d.d_name pp_opt_type d.d_type pp_opt_value d.d_value
    ) decls
  | STypes (decls, _) ->
    List.iter (fun d ->
      pp fmt "(types %s%a)@\n" d.d_name pp_opt_type d.d_type
    ) decls
  | SStatics (decls, _) ->
    List.iter (fun d ->
      pp fmt "(statics %s%a%a)@\n" d.d_name pp_opt_type d.d_type pp_opt_value d.d_value
    ) decls
  | SFieldSymbols (decls, _) ->
    List.iter (fun d ->
      pp fmt "(field-symbols <%s>%a)@\n" d.d_name pp_opt_type d.d_type
    ) decls

  | SAssign (lhs, rhs, _) -> pp fmt "(assign %a = %a)" pp_expr lhs pp_expr rhs
  | SCastAssign (lhs, rhs, _) -> pp fmt "(assign %a ?= %a)" pp_expr lhs pp_expr rhs
  | SInlineDecl (name, rhs, _) -> pp fmt "(inline-data %s = %a)" name pp_expr rhs

  | SIf (block, _) ->
    pp fmt "@[<v 2>(if %a@\n%a" pp_expr block.if_cond pp_body block.if_then;
    List.iter (fun (cond, body) ->
      pp fmt "(elseif %a@\n" pp_expr cond;
      List.iter (fun s -> pp fmt "  %a@\n" pp_stmt s) body;
      pp fmt ")@\n") block.if_elseifs;
    (match block.if_else with
     | Some body ->
       pp fmt "(else@\n";
       List.iter (fun s -> pp fmt "  %a@\n" pp_stmt s) body;
       pp fmt ")@\n"
     | None -> ());
    pp fmt ")@]"

  | SCase (e, whens, others, _) ->
    pp fmt "@[<v 2>(case %a@\n" pp_expr e;
    List.iter (fun w ->
      pp fmt "(when";
      List.iter (fun v -> pp fmt " %a" pp_expr v) w.cw_values;
      pp fmt "@\n";
      List.iter (fun s -> pp fmt "  %a@\n" pp_stmt s) w.cw_body;
      pp fmt ")@\n") whens;
    (match others with
     | Some body ->
       pp fmt "(others@\n";
       List.iter (fun s -> pp fmt "  %a@\n" pp_stmt s) body;
       pp fmt ")@\n"
     | None -> ());
    pp fmt ")@]"

  | SDo (times, body, _) ->
    pp fmt "@[<v 2>(do";
    (match times with Some e -> pp fmt " %a TIMES" pp_expr e | None -> ());
    pp fmt "@\n%a)@]" pp_body body

  | SWhile (cond, body, _) ->
    pp fmt "@[<v 2>(while %a@\n%a)@]" pp_expr cond pp_body body

  | SLoop (spec, body, _) ->
    pp fmt "@[<v 2>(loop";
    (match spec with LoopAt e -> pp fmt " AT %a" pp_expr e | LoopGeneric -> ());
    pp fmt "@\n%a)@]" pp_body body

  | SCheck (e, _) -> pp fmt "(check %a)" pp_expr e
  | SContinue _ -> pp fmt "(continue)"
  | SExit _ -> pp fmt "(exit)"
  | SReturn _ -> pp fmt "(return)"
  | SStop _ -> pp fmt "(stop)"

  | SWrite (items, _) ->
    pp fmt "(write";
    List.iter (function
      | WSlash -> pp fmt " /"
      | WExpr e -> pp fmt " %a" pp_expr e
      | WSpec (s, e) -> pp fmt " (%s %a)" s pp_expr e) items;
    pp fmt ")"

  | SClear (e, _) -> pp fmt "(clear %a)" pp_expr e
  | SFree (e, _) -> pp fmt "(free %a)" pp_expr e
  | SAssignTo (src, fs, _) -> pp fmt "(assign %a TO %a)" pp_expr src pp_expr fs

  | SAppend (e, target, _) ->
    pp fmt "(append %a" pp_expr e;
    (match target with Some t -> pp fmt " TO %a" pp_expr t | None -> ());
    pp fmt ")"
  | SInsert (e, target, _) ->
    pp fmt "(insert %a" pp_expr e;
    (match target with Some t -> pp fmt " INTO %a" pp_expr t | None -> ());
    pp fmt ")"

  | SDelete (e, _) -> pp fmt "(delete %a)" pp_expr e
  | SSort (e, _) -> pp fmt "(sort %a)" pp_expr e
  | SRead _ -> pp fmt "(read ...)"
  | SModify _ -> pp fmt "(modify ...)"

  | SMove (src, dst, _) ->
    pp fmt "(move %a TO %a)" pp_expr src pp_expr dst
  | SMoveCorr (src, dst, _) ->
    pp fmt "(move-corresponding %a TO %a)" pp_expr src pp_expr dst

  | SForm (name, using, changing, body, _) ->
    pp fmt "@[<v 2>(form %s" name;
    if using <> [] then (pp fmt " USING"; List.iter (pp fmt " %s") using);
    if changing <> [] then (pp fmt " CHANGING"; List.iter (pp fmt " %s") changing);
    pp fmt "@\n%a)@]" pp_body body

  | SPerform (name, args, _) ->
    pp fmt "(perform %s" name;
    List.iter (fun e -> pp fmt " %a" pp_expr e) args;
    pp fmt ")"

  | SClassDef (def, _) ->
    pp fmt "@[<v 2>(class-definition %s" def.cls_name;
    (match def.cls_super with Some s -> pp fmt " INHERITING FROM %s" s | None -> ());
    pp fmt "@\n";
    let pp_section name stmts =
      if stmts <> [] then begin
        pp fmt "(%s@\n" name;
        List.iter (fun s -> pp fmt "  %a@\n" pp_stmt s) stmts;
        pp fmt ")@\n"
      end
    in
    pp_section "public" def.cls_public;
    pp_section "protected" def.cls_protected;
    pp_section "private" def.cls_private;
    pp fmt ")@]"

  | SClassImpl (name, body, _) ->
    pp fmt "@[<v 2>(class-implementation %s@\n%a)@]" name pp_body body
  | SMethod (name, body, _) ->
    pp fmt "@[<v 2>(method %s@\n%a)@]" name pp_body body
  | SInterface (name, body, _) ->
    pp fmt "@[<v 2>(interface %s@\n%a)@]" name pp_body body

  | STry (body, catches, cleanup, _) ->
    pp fmt "@[<v 2>(try@\n%a" pp_body body;
    List.iter (fun c ->
      pp fmt "(catch";
      List.iter (pp fmt " %s") c.catch_types;
      (match c.catch_into with Some v -> pp fmt " INTO %s" v | None -> ());
      pp fmt "@\n";
      List.iter (fun s -> pp fmt "  %a@\n" pp_stmt s) c.catch_body;
      pp fmt ")@\n") catches;
    (match cleanup with
     | Some body ->
       pp fmt "(cleanup@\n";
       List.iter (fun s -> pp fmt "  %a@\n" pp_stmt s) body;
       pp fmt ")@\n"
     | None -> ());
    pp fmt ")@]"

  | SRaise (e, _) -> pp fmt "(raise %a)" pp_expr e
  | SSelect _ -> pp fmt "(select ...)"
  | SMessage _ -> pp fmt "(message ...)"
  | SCreateObject (target, args, _) ->
    pp fmt "(create-object %a" pp_expr target;
    List.iter (fun a -> pp fmt " %a" pp_named_arg a) args;
    pp fmt ")"
  | SCreateData (target, _) -> pp fmt "(create-data %a)" pp_expr target
  | SCallFunction (name, args, _) ->
    pp fmt "(call-function '%s'" name;
    List.iter (fun a -> pp fmt " %a" pp_named_arg a) args;
    pp fmt ")"
  | SCallMethod (target, args, _) ->
    pp fmt "(call-method %a" pp_expr target;
    List.iter (fun a -> pp fmt " %a" pp_named_arg a) args;
    pp fmt ")"
  | SExprStmt (e, _) -> pp fmt "(expr %a)" pp_expr e
  | SUnknown (kw, _toks, _) -> pp fmt "(unknown %s ...)" kw

let pp_program fmt prog =
  pp fmt "@[<v>(program@\n";
  List.iter (fun s -> pp fmt "  %a@\n" pp_stmt s) prog.p_stmts;
  pp fmt ")@]@."
