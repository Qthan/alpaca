type token =
  | T_EOF
  | T_ANDDEF
  | T_ARRAY
  | T_BEGIN
  | T_DELETE
  | T_DIM
  | T_DO
  | T_DONE
  | T_DOWNTO
  | T_ELSE
  | T_END
  | T_FOR
  | T_IF
  | T_IN
  | T_LET
  | T_MATCH
  | T_MUTABLE
  | T_NEW
  | T_NOT
  | T_OF
  | T_REC
  | T_REF
  | T_THEN
  | T_TO
  | T_TYPE
  | T_WHILE
  | T_WITH
  | T_ID of (string)
  | T_CID of (string)
  | T_FLOAT of (float)
  | T_INT of (int)
  | T_CHAR
  | T_CONSTCHAR of (char)
  | T_STRING of (string)
  | T_FALSE of (bool)
  | T_TRUE of (bool)
  | T_BOOL
  | T_UNIT
  | T_FLOATST
  | T_INTST
  | T_GIVES
  | T_SEQ
  | T_BAR
  | T_PLUS
  | T_MINUS
  | T_TIMES
  | T_DIV
  | T_MOD
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

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> unit
