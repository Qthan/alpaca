{
open Parser
open Lexing

exception EOF of string
(*:/
type token =
    T_ANDDEF
  | T_ARRAY
  | T_EOF
  | T_BEGIN
  | T_BOOL
  | T_CHAR
  | T_DELETE
  | T_DIM
  | T_DO
  | T_DONE
  | T_DOWNTO
  | T_EOL
  | T_ELSE
  | T_END
  | T_FALSE
  | T_FLOAT
  | T_FOR
  | T_IF
  | T_IN
  | T_INT
  | T_LET
  | T_MATCH
  | T_MOD
  | T_MUTABLE
  | T_NEW
  | T_NOT
  | T_OF
  | T_REC
  | T_REF
  | T_THEN
  | T_TO
  | T_TRUE
  | T_TYPE
  | T_UNIT
  | T_WHILE
  | T_WITH
  | T_ID 
  | T_CID 
  | T_OP 
  | T_GIVES
  | T_SEQ 
  | T_PLUS 
  | T_MINUS
  | T_STRING
  | T_TIMES
  | T_DIV
  | T_FPLUS
  | T_FMINUS
  | T_FTIMES
  | T_FDIV
  | T_POWER
  | T_BANK
  | T_SMCOLON
  | T_AND
  | T_OR
  | T_NSEQ
  | T_L
  | T_G
  | T_LE
  | T_GE
  | T_EQ
  | T_NEQ
  | T_ASSIGN
  | T_LPAR
  | T_RPAR
  | T_LBRACK
  | T_RBRACK
  | T_COMA
  | T_COLON

*)

let incr_linenum lexbuf =
  let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    } 

let print_error lexbuf chr = 
  let pos = lexbuf.lex_curr_p in
  Printf.printf "error::line:%d-character:%d:->Invalid character '%c' (ascii: %d)\n" 
    (pos.pos_lnum) (pos.pos_cnum - pos.pos_bol) (chr) (Char.code chr)

}

let digit     = ['0'-'9']
let hexdig    = ['0'-'9''a'-'f''A'-'F']
let upCase    = ['A'-'Z']
let lowCase   = ['a'-'z']
let id        = ( upCase | lowCase | '_' | digit )
let white     = ['\t' ' ']
let constChar = ( [^ '\\' '\"' '\''] ) | ( '\\'( ( ['n' 't' 'r' '0' '\\' '\'' '\"'] ) | ( 'x' hexdig hexdig ) ) )


rule lexer = parse
  | white               { lexer lexbuf }
  | ['\n']              { incr_linenum lexbuf; lexer lexbuf  (*; EOL*) }
  | digit+              { T_INT } 
  | digit+( '.'digit+('e'['-''+']?digit+)? ) 
                        { T_FLOAT }
  | "and"               { T_ANDDEF }
  | "array"             { T_ARRAY }
  | "begin"             { T_BEGIN } 
  | "bool"              { T_BOOL }
  | "char"              { T_CHAR }
  | "delete"            { T_DELETE }
  | "dim"               { T_DIM }
  | "do"                { T_DO }
  | "done"              { T_DONE }
  | "downto"            { T_DOWNTO }
  | "else"              { T_ELSE }
  | "end"               { T_END }
  | "false"             { T_FALSE }
  | "float"             { T_FLOATST }
  | "for"               { T_FOR }
  | "if"                { T_IF }
  | "in"                { T_IN }
  | "int"               { T_INTST }
  | "let"               { T_LET }
  | "match"             { T_MATCH }
  | "mod"               { T_MOD }
  | "mutable"           { T_MUTABLE }
  | "new"               { T_NEW }
  | "not"               { T_NOT }
  | "of"                { T_OF }
  | "rec"               { T_REC }
  | "ref"               { T_REF }
  | "then"              { T_THEN }
  | "to"                { T_TO }
  | "true"              { T_TRUE }
  | "type"              { T_TYPE }
  | "unit"              { T_UNIT }
  | "while"             { T_WHILE }
  | "with"              { T_WITH }
  | "->"                { T_GIVES }
  | "="                 { T_SEQ }
  | "|"                 { T_BAR }
  | "+"                 { T_PLUS }
  | "-"                 { T_MINUS }
  | "*"                 { T_TIMES }
  | "/"                 { T_DIV }
  | "+."                { T_FPLUS } 
  | "-."                { T_FMINUS }
  | "*."                { T_FTIMES }
  | "/."                { T_FDIV }
  | "**"                { T_POWER }
  | "!"                 { T_BANK }
  | ";"                 { T_SMCOLON }
  | "&&"                { T_AND }
  | "||"                { T_OR }
  | "<>"                { T_NSEQ }
  | "<"                 { T_L }
  | ">"                 { T_G }
  | "<="                { T_LE }
  | ">="                { T_GE }
  | "=="                { T_EQ }
  | "!="                { T_NEQ }
  | ":="                { T_ASSIGN }
  | "("                 { T_LPAR }
  | ")"                 { T_RPAR }
  | "["                 { T_LBRACK }
  | "]"                 { T_RBRACK }
  | ","                 { T_COMA }
  | ":"                 { T_COLON }
  | lowCase+id*         { T_ID }
  | upCase+id*          { T_CID }
  | '\''constChar '\''  { T_CONSTCHAR }
  | '\"'[^'\n']* '\"'   { T_STRING }
  | "--"[^'\n']*        { lexer lexbuf } 
  | "(*"                { print_endline "comments start"; comments 0 lexbuf }
  | _ as chr            { print_error lexbuf chr ; lexer lexbuf }
  | eof                 { T_EOF }

and comments level = parse
  | "*)"                { Printf.printf "comments (%d) end\n" level;
	                      if level = 0 then lexer lexbuf
	                        else comments (level-1) lexbuf
	                    }
  | "(*"                { Printf.printf "comments (%d) start\n" (level+1);
                          comments (level+1) lexbuf
	                    }
  | '\n'                { incr_linenum lexbuf ; comments level lexbuf }
  | _                   { comments level lexbuf  }
  | eof                 { raise ( EOF "File ended before comments were closed") }

{
 (* 
  let string_of_token token =
    match token with
  | T_ANDDEF  -> "ANDDEF"
  | T_ARRAY   -> "ARRAY"
  | T_BEGIN   -> "BEGIN"
  | T_BOOL    -> "BOOL"
  | T_DELETE  -> "DELETE"
  | T_DIM     -> "DIM"
  | T_DO      -> "DO"
  | T_DONE    -> "DONE"
  | T_DOWNTO  -> "DOWNTO"
  | T_STRING  -> "STRING"
  | T_ELSE    -> "ELSE"
  | T_END     -> "END"
  | T_FALSE   -> "FALSE"
  | T_FOR     -> "FOR"
  | T_IF      -> "IF"
  | T_IN      -> "IN"
  | T_LET     -> "LET"
  | T_MOD     -> "MOD"
  | T_MUTABLE -> "MUTABLE"
  | T_NEW     -> "NEW"
  | T_NOT     -> "NOT"
  | T_OF      -> "OF"
  | T_REC     -> "REC"
  | T_REF     -> "REF"
  | T_THEN    -> "THEN"
  | T_TO      -> "TO"
  | T_TRUE    -> "TRUE"
  | T_TYPE    -> "TYPE"
  | T_UNIT    -> "UNIT"
  | T_WHILE   -> "WHILE"
  | T_WITH    -> "WITH"
  | T_ID      -> "ID"
  | T_CID     -> "CID"
  | T_OP      -> "OP"
  | T_INT     -> "INT"
  | T_FLOAT   -> "FLOAT"
  | T_CHAR    -> "CHAR"
  | T_GIVES   -> "GIVES"
  | T_SEQ     -> "SEQ"
  | T_MATCH   -> "MATCH"
  | T_PLUS    -> "PLUS"
  | T_MINUS   -> "MINUS"
  | T_TIMES   -> "TIMES"
  | T_DIV     -> "DIV"
  | T_FPLUS   -> "FPLUS"
  | T_FMINUS  -> "FMINUS"
  | T_FTIMES  -> "FTIMES"
  | T_FDIV    -> "FDIV"
  | T_POWER   -> "POWER"
  | T_EOL     -> "EOL"
  | T_BANK    -> "BANK"
  | T_SMCOLON -> "SMCOLON"
  | T_AND     -> "AND"
  | T_OR      -> "OR"
  | T_NSEQ    -> "NSEQ"
  | T_L       -> "L"
  | T_G       -> "G"
  | T_LE      -> "LE"
  | T_GE      -> "GE"
  | T_EQ      -> "EQ"
  | T_NEQ     -> "NEQ"
  | T_ASSIGN  -> "ASSIGN"
  | T_LPAR    -> "LPAR"
  | T_RPAR    -> "RPAR"
  | T_LBRACK  -> "LBRACK"
  | T_RBRACK  -> "RBRACK"
  | T_COMA    -> "COMA"
  | T_COLON   -> "COLON"
  | T_EOF     -> "EOF"


let incr_linenum lexbuf =
  let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

let main =
  let cin =
    if Array.length Sys.argv >1
    then open_in Sys.argv.(1)
    else stdin
  in
  let lexbuf = from_channel cin in
  let rec loop () =
    let token = token lexbuf in
        Printf.printf "line=%d\t token=%s\t lexeme= %s\n"
          ( lexbuf.lex_curr_p.pos_lnum )
          ( string_of_token token ) 
          ( lexeme lexbuf );
        if token <> T_EOF then loop () in
  loop ()
*)
}

