{ 
exception Eof 

type token =
  | AND
  | ARRAY
  | DIM
  | DO
  | FALSE
  | FLOAT
  | LET
  | MATCH
  | OF
  | REC
  | TYPE
  | UNIT
  | BEGIN
  | BOOL
  | CHAR
  | DELETE
  | DONE
  | DOWN TO
  | ELSE
  | END
  | FOR
  | IF
  | IN
  | INT
  | MOD
  | MUTABLE
  | NEW
  | NOT
  | REF
  | THEN
  | TO
  | TRUE
  | WHILE
  | WITH
  | ID of string
  | OP of char
  | INT of int
  | FLOAT of float

and array dim do false float let match of rec type unit
    begin bool char delete done downto else end for if in int mod mutable new not ref then to true while with

}

let digit = ['0'-'9']
let upCase = ['A'-'Z']
let lowCase = ['a'-'z']
let white = ['\t' ' '] 
let num_lines = ref 0

rule token = parse
    white { token lexbuf }
  | ['\n'] { incr num_lines; EOL; }
  | digit+ as inum { INT(int_of_string inum) }
  | digit+ ( \. digit+ ( e [\- \+]? digit+ )? ) as fnum { FLOAT( float_to_string fnum ) }
  | 
