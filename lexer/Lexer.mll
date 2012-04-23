{
open Parser
open Lexing

exception EOF of string

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
  | "(*"                {  comments 0 lexbuf }
  | _ as chr            { print_error lexbuf chr ; lexer lexbuf }
  | eof                 { T_EOF }

and comments level = parse
  | "*)"                { 
	                      if level = 0 then lexer lexbuf
	                        else comments (level-1) lexbuf
	                    }
  | "(*"                { 
                          comments (level+1) lexbuf
	                    }
  | '\n'                { incr_linenum lexbuf ; comments level lexbuf }
  | _                   { comments level lexbuf  }
  | eof                 { raise ( EOF "File ended before comments were closed") }


