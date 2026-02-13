(* ABAP AST — the language where every keyword is also a valid variable name,
   every statement ends with a full stop like a passive-aggressive email,
   and hyphens are load-bearing structural members. *)

type table_kind = Standard | Sorted | Hashed

type type_spec =
  | TSimple of string
  | TRef of string
  | TLike of string
  | TTable of table_kind * type_spec
  | TStruct of field list

and field = {
  f_name : string;
  f_type : type_spec;
  f_loc  : Loc.t;
}

type data_decl = {
  d_name  : string;
  d_type  : type_spec option;
  d_value : expr option;
  d_loc   : Loc.t;
}

and binop =
  | Add | Sub | Mul | Div | Mod | Power
  | Concat
  | And | Or
  | Eq | Ne | Lt | Gt | Le | Ge
  | CO | CN | CA | NA | CS | NS | CP | NP
  | In | Between

and unop = UPlus | UMinus | Not

and expr =
  | EInt of string * Loc.t
  | EFloat of string * Loc.t
  | EString of string * Loc.t
  | EBString of string * Loc.t
  | ETemplate of template_part list * Loc.t
  | EIdent of string * Loc.t
  | EFieldSymbol of string * Loc.t
  | EBinOp of binop * expr * expr * Loc.t
  | EUnOp of unop * expr * Loc.t
  | EArrow of expr * string * Loc.t
  | EDArrow of expr * string * Loc.t
  | ETilde of expr * string * Loc.t
  | ECall of expr * named_arg list * Loc.t
  | ETableExpr of expr * expr list * Loc.t
  | ENew of type_or_hash * named_arg list * Loc.t
  | EValue of type_or_hash * value_body list * Loc.t
  | EConv of type_or_hash * named_arg list * Loc.t
  | ECast of type_or_hash * named_arg list * Loc.t
  | ECond of type_or_hash * cond_branch list * expr option * Loc.t
  | ESwitch of type_or_hash * expr * cond_branch list * expr option * Loc.t
  | ERef of type_or_hash * named_arg list * Loc.t
  | ECorresponding of type_or_hash * named_arg list * Loc.t
  | EIs of expr * is_test * Loc.t
  | EParen of expr * Loc.t
  | EStar of Loc.t

and template_part =
  | TPText of string
  | TPExpr of expr

and type_or_hash =
  | THType of string
  | THHash

and named_arg =
  | Positional of expr
  | Named of string * expr
  | NamedExporting of string * expr
  | NamedImporting of string * expr
  | NamedChanging of string * expr
  | NamedReceiving of string * expr

and value_body =
  | VBArg of named_arg
  | VBRow of named_arg list

and cond_branch = {
  cb_cond : expr;
  cb_then : expr;
}

and is_test =
  | IsInitial
  | IsBound
  | IsAssigned
  | IsSupplied
  | IsInstance of string
  | IsNotInitial
  | IsNotBound
  | IsNotAssigned

type write_item =
  | WExpr of expr
  | WSlash
  | WSpec of string * expr

type stmt =
  | SComment of string * Loc.t
  | SPragma of string * Loc.t
  | SReport of string * Loc.t
  | SProgram of string * Loc.t
  | SInclude of string * Loc.t
  | SData of data_decl list * Loc.t
  | SConstants of data_decl list * Loc.t
  | STypes of data_decl list * Loc.t
  | SStatics of data_decl list * Loc.t
  | SFieldSymbols of data_decl list * Loc.t
  | SAssign of expr * expr * Loc.t
  | SCastAssign of expr * expr * Loc.t
  | SInlineDecl of string * expr * Loc.t
  | SIf of if_block * Loc.t
  | SCase of expr * case_when list * stmt list option * Loc.t
  | SDo of expr option * stmt list * Loc.t
  | SWhile of expr * stmt list * Loc.t
  | SLoop of loop_spec * stmt list * Loc.t
  | SCheck of expr * Loc.t
  | SContinue of Loc.t
  | SExit of Loc.t
  | SReturn of Loc.t
  | SStop of Loc.t
  | SWrite of write_item list * Loc.t
  | SClear of expr * Loc.t
  | SFree of expr * Loc.t
  | SAssignTo of expr * expr * Loc.t
  | SAppend of expr * expr option * Loc.t
  | SInsert of expr * expr option * Loc.t
  | SDelete of expr * Loc.t
  | SSort of expr * Loc.t
  | SRead of Loc.t
  | SModify of Loc.t
  | SMove of expr * expr * Loc.t
  | SMoveCorr of expr * expr * Loc.t
  | SForm of string * string list * string list * stmt list * Loc.t
  | SPerform of string * expr list * Loc.t
  | SClassDef of class_def * Loc.t
  | SClassImpl of string * stmt list * Loc.t
  | SMethod of string * stmt list * Loc.t
  | SInterface of string * stmt list * Loc.t
  | STry of stmt list * catch_block list * stmt list option * Loc.t
  | SRaise of expr * Loc.t
  | SSelect of Loc.t
  | SMessage of Loc.t
  | SCreateObject of expr * named_arg list * Loc.t
  | SCreateData of expr * Loc.t
  | SCallFunction of string * named_arg list * Loc.t
  | SCallMethod of expr * named_arg list * Loc.t
  | SExprStmt of expr * Loc.t
  | SUnknown of string * (Token.t * Loc.t) list * Loc.t

and if_block = {
  if_cond    : expr;
  if_then    : stmt list;
  if_elseifs : (expr * stmt list) list;
  if_else    : stmt list option;
}

and case_when = {
  cw_values : expr list;
  cw_body   : stmt list;
}

and loop_spec =
  | LoopAt of expr
  | LoopGeneric

and catch_block = {
  catch_types : string list;
  catch_into  : string option;
  catch_body  : stmt list;
}

and class_def = {
  cls_name      : string;
  cls_super     : string option;
  cls_public    : stmt list;
  cls_private   : stmt list;
  cls_protected : stmt list;
}

type program = {
  p_stmts : stmt list;
}
