(* ABAP token types.

   Keywords are NOT reserved in ABAP. The lexer emits IDENT for everything
   and the parser disambiguates based on context. A separate keyword lookup
   is provided for the parser to check when it expects a keyword. *)

type t =
  (* Literals *)
  | INT of string           (* 123, +5, -3 *)
  | FLOAT of string         (* 1.5, -3.14 *)
  | STRING of string        (* 'single quoted' text field *)
  | BSTRING of string       (* `backtick quoted` text string *)
  | TEMPLATE_BEGIN          (* | to start string template *)
  | TEMPLATE_END            (* | to end string template *)
  | TEMPLATE_TEXT of string (* literal text within template *)
  | TEMPLATE_EXPR_BEGIN     (* { within template *)
  | TEMPLATE_EXPR_END       (* } within template *)

  (* Identifiers — keywords are also emitted as IDENT *)
  | IDENT of string         (* uppercased *)

  (* Punctuation *)
  | DOT                     (* . statement terminator *)
  | COMMA                   (* , chain separator *)
  | COLON                   (* : chain introducer *)
  | LPAREN                  (* ( *)
  | RPAREN                  (* ) *)
  | LBRACKET                (* [ table expression *)
  | RBRACKET                (* ] *)

  (* Operators *)
  | PLUS                    (* + *)
  | MINUS                   (* - also component access *)
  | STAR                    (* * also full-line comment indicator *)
  | SLASH                   (* / *)
  | POWER                   (* ** *)
  | AMPAMP                  (* && string concat *)
  | EQ                      (* = *)
  | NE                      (* <> *)
  | LT                      (* < *)
  | GT                      (* > *)
  | LE                      (* <= *)
  | GE                      (* >= *)
  | ARROW                   (* -> instance access *)
  | DARROW                  (* => static access *)
  | AT                      (* @ SQL host variable *)
  | HASH                    (* # for type inference *)
  | AMP                     (* & literal concatenation *)
  | TILDE                   (* ~ interface component access *)
  | CAST_EQ                  (* ?= casting assignment *)

  (* Pragma *)
  | PRAGMA of string         (* ##identifier *)

  (* Special *)
  | FIELD_SYMBOL of string  (* <fs> *)

  (* Meta *)
  | COMMENT of string       (* preserved for formatting tools *)
  | EOF

(* Keyword lookup — case-insensitive, input should be uppercased *)
let keyword_set = [
  "AND"; "APPEND"; "ASSIGN"; "ASSIGNING"; "AT";
  "BEGIN"; "BINARY";
  "CALL"; "CASE"; "CAST"; "CATCH"; "CHANGING"; "CHECK"; "CLASS";
  "CLASS-DATA"; "CLASS-METHODS"; "CLEAR"; "CLOSE"; "COLLECT";
  "COMMIT"; "COND"; "CONSTANTS"; "CONTINUE"; "CONV"; "CORRESPONDING";
  "CREATE"; "DATA"; "DEFAULT"; "DEFINE"; "DELETE"; "DESCRIBE";
  "DISTINCT"; "DIV"; "DO";
  "ELSE"; "ELSEIF"; "END-OF-DEFINITION"; "END-OF-PAGE";
  "END-OF-SELECTION"; "ENDCASE"; "ENDCLASS"; "ENDDO"; "ENDFORM";
  "ENDFUNCTION"; "ENDIF"; "ENDINTERFACE"; "ENDLOOP"; "ENDMETHOD";
  "ENDMODULE"; "ENDSELECT"; "ENDTRY"; "ENDWHILE"; "EQ"; "EVENT";
  "EVENTS"; "EXCEPTIONS"; "EXIT"; "EXPORTING";
  "FIELD-SYMBOLS"; "FIELDS"; "FINAL"; "FOR"; "FORM"; "FREE";
  "FROM"; "FUNCTION";
  "GE"; "GET"; "GROUP"; "GT";
  "HASHED"; "HAVING";
  "IF"; "IMPLEMENTATION"; "IMPORTING"; "IN"; "INCLUDE"; "INDEX";
  "INHERITING"; "INITIAL"; "INITIALIZATION"; "INSERT"; "INTERFACE";
  "INTERFACES"; "INTO"; "IS";
  "JOIN";
  "KEY";
  "LE"; "LIKE"; "LINE"; "LINES"; "LOOP"; "LT";
  "MESSAGE"; "METHOD"; "METHODS"; "MOD"; "MODIFY"; "MODULE"; "MOVE";
  "NE"; "NEW"; "NON-UNIQUE"; "NOT";
  "OBLIGATORY"; "OCCURRENCE"; "OCCURRENCES"; "OF"; "ON"; "OPEN";
  "OPTIONAL"; "OR"; "ORDER"; "OTHERS"; "OUTPUT";
  "PARAMETERS"; "PERFORM"; "PRIVATE"; "PROGRAM"; "PROTECTED";
  "PUBLIC";
  "RAISE"; "RAISING"; "READ"; "RECEIVING"; "REF"; "REFERENCE";
  "REFRESH"; "REPORT"; "RESULT"; "RETURN"; "RETURNING"; "ROLLBACK";
  "SEARCH"; "SECTION"; "SELECT"; "SELECT-OPTIONS"; "SINGLE";
  "SORT"; "SORTED"; "SPLIT"; "STANDARD"; "START-OF-SELECTION";
  "STATICS"; "STOP"; "STRING"; "STRUCTURE"; "SUBMIT"; "SWITCH";
  "TABLE"; "TABLES"; "THEN"; "TO"; "TOP-OF-PAGE"; "TRY"; "TYPE";
  "TYPES";
  "UNION"; "UNIQUE"; "UP"; "UPDATE"; "USING";
  "VALUE";
  "WHEN"; "WHERE"; "WHILE"; "WITH"; "WORK"; "WRITE";
]

let keywords =
  let tbl = Hashtbl.create 256 in
  List.iter (fun kw -> Hashtbl.replace tbl kw true) keyword_set;
  tbl

let is_keyword s = Hashtbl.mem keywords (String.uppercase_ascii s)

let pp fmt = function
  | INT s -> Format.fprintf fmt "INT(%s)" s
  | FLOAT s -> Format.fprintf fmt "FLOAT(%s)" s
  | STRING s -> Format.fprintf fmt "STRING('%s')" s
  | BSTRING s -> Format.fprintf fmt "BSTRING(`%s`)" s
  | TEMPLATE_BEGIN -> Format.fprintf fmt "TMPL_BEGIN"
  | TEMPLATE_END -> Format.fprintf fmt "TMPL_END"
  | TEMPLATE_TEXT s -> Format.fprintf fmt "TMPL_TEXT(%s)" s
  | TEMPLATE_EXPR_BEGIN -> Format.fprintf fmt "TMPL_EXPR{"
  | TEMPLATE_EXPR_END -> Format.fprintf fmt "TMPL_EXPR}"
  | IDENT s -> Format.fprintf fmt "IDENT(%s)" s
  | DOT -> Format.fprintf fmt "DOT"
  | COMMA -> Format.fprintf fmt "COMMA"
  | COLON -> Format.fprintf fmt "COLON"
  | LPAREN -> Format.fprintf fmt "LPAREN"
  | RPAREN -> Format.fprintf fmt "RPAREN"
  | LBRACKET -> Format.fprintf fmt "LBRACKET"
  | RBRACKET -> Format.fprintf fmt "RBRACKET"
  | PLUS -> Format.fprintf fmt "PLUS"
  | MINUS -> Format.fprintf fmt "MINUS"
  | STAR -> Format.fprintf fmt "STAR"
  | SLASH -> Format.fprintf fmt "SLASH"
  | POWER -> Format.fprintf fmt "POWER"
  | AMPAMP -> Format.fprintf fmt "AMPAMP"
  | AMP -> Format.fprintf fmt "AMP"
  | EQ -> Format.fprintf fmt "EQ"
  | NE -> Format.fprintf fmt "NE"
  | LT -> Format.fprintf fmt "LT"
  | GT -> Format.fprintf fmt "GT"
  | LE -> Format.fprintf fmt "LE"
  | GE -> Format.fprintf fmt "GE"
  | ARROW -> Format.fprintf fmt "ARROW"
  | DARROW -> Format.fprintf fmt "DARROW"
  | AT -> Format.fprintf fmt "AT"
  | HASH -> Format.fprintf fmt "HASH"
  | TILDE -> Format.fprintf fmt "TILDE"
  | CAST_EQ -> Format.fprintf fmt "CAST_EQ"
  | PRAGMA s -> Format.fprintf fmt "PRAGMA(%s)" s
  | FIELD_SYMBOL s -> Format.fprintf fmt "FIELD_SYMBOL(<%s>)" s
  | COMMENT s -> Format.fprintf fmt "COMMENT(%s)" s
  | EOF -> Format.fprintf fmt "EOF"
