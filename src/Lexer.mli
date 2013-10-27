exception EOF of string
val incr_linenum : Lexing.lexbuf -> unit
val print_error : Lexing.lexbuf -> char -> unit
val get_pos : Lexing.lexbuf -> int * int
val __ocaml_lex_tables : Lexing.lex_tables
val lexer : Lexing.lexbuf -> Parser.token
val __ocaml_lex_lexer_rec : Lexing.lexbuf -> int -> Parser.token
val comments : int -> Lexing.lexbuf -> Parser.token
val __ocaml_lex_comments_rec : int -> Lexing.lexbuf -> int -> Parser.token
