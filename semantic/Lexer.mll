{
open Parser
open Lexing
open Types

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

let get_pos lexbuf =
    let pos = lexbuf.lex_curr_p in
        (pos.pos_lnum, pos.pos_cnum - pos.pos_bol)


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
  | digit+ as num       { T_INT { ival = (int_of_string num); ipos = get_pos lexbuf } } 
  | digit+( '.'digit+('e'['-''+']?digit+)? ) as fnum
                        { T_FLOAT { fval = (float_of_string fnum ); fpos = get_pos lexbuf } }
  | "and"               { T_ANDDEF }
  | "array"             { T_ARRAY }
  | "begin"             { T_BEGIN { pos = get_pos lexbuf } } 
  | "bool"              { T_BOOL }
  | "char"              { T_CHAR }
  | "delete"            { T_DELETE { pos = get_pos lexbuf } }
  | "dim"               { T_DIM { pos = get_pos lexbuf } }
  | "do"                { T_DO }
  | "done"              { T_DONE }
  | "downto"            { T_DOWNTO }
  | "else"              { T_ELSE }
  | "end"               { T_END }
  | "false"             { T_FALSE { bval = false; bpos = get_pos lexbuf } }
  | "float"             { T_FLOATST }
  | "for"               { T_FOR { pos = get_pos lexbuf } }
  | "if"                { T_IF { pos = get_pos lexbuf } }
  | "in"                { T_IN }
  | "int"               { T_INTST }
  | "let"               { T_LET }
  | "match"             { T_MATCH { pos = get_pos lexbuf } }
  | "mod"               { T_MOD }
  | "mutable"           { T_MUTABLE }
  | "new"               { T_NEW { pos = get_pos lexbuf } }
  | "not"               { T_NOT { pos = get_pos lexbuf } }
  | "of"                { T_OF }
  | "rec"               { T_REC }
  | "ref"               { T_REF }
  | "then"              { T_THEN }
  | "to"                { T_TO }
  | "true"              { T_TRUE { bval = true; bpos = get_pos lexbuf } }
  | "type"              { T_TYPE }
  | "unit"              { T_UNIT }
  | "while"             { T_WHILE { pos = get_pos lexbuf } }
  | "with"              { T_WITH }
  | "->"                { T_GIVES }
  | "="                 { T_SEQ } 
  | "|"                 { T_BAR }
  | "+"                 { T_PLUS { pos = get_pos lexbuf } }
  | "-"                 { T_MINUS { pos = get_pos lexbuf } }
  | "*"                 { T_TIMES }
  | "/"                 { T_DIV }
  | "+."                { T_FPLUS { pos = get_pos lexbuf } } 
  | "-."                { T_FMINUS { pos = get_pos lexbuf } }
  | "*."                { T_FTIMES }
  | "/."                { T_FDIV }
  | "**"                { T_POWER }
  | "!"                 { T_BANK { pos = get_pos lexbuf } }
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
  | "("                 { T_LPAR { pos = get_pos lexbuf } }
  | ")"                 { T_RPAR }
  | "["                 { T_LBRACK }
  | "]"                 { T_RBRACK }
  | ","                 { T_COMA }
  | ":"                 { T_COLON }
  | lowCase+id* as id   { T_ID { id_name = id; id_pos = get_pos lexbuf } }
  | upCase+id* as cid   { T_CID { cid_name = cid; cid_pos = get_pos lexbuf } }
  | '\'' (constChar as c_char) '\''  
                        { T_CONSTCHAR { cval = c_char; cpos = get_pos lexbuf } }
  | '\"' (([^'\n' '\"'] | '\\' '\"')* as c_string) '\"'  
                        { T_STRING { sval = c_string; spos = get_pos lexbuf } }
  | "--"[^'\n']*        { lexer lexbuf } 
  | "(*"                { comments 0 lexbuf }
  | _ as chr            { print_error lexbuf chr ; lexer lexbuf }
  | eof                 { T_EOF }

and comments level = parse
  | "*)"                { 
                          if level = 0 
                          then lexer lexbuf
	                      else comments (level-1) lexbuf
	                    }
  | "(*"                { 
                          comments (level+1) lexbuf
	                    }
  | '\n'                { incr_linenum lexbuf ; comments level lexbuf }
  | _                   { comments level lexbuf  }
  | eof                 { raise ( EOF "File ended before comments were closed") }
