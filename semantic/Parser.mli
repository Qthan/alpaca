type token =
  | T_EOF
  | T_ANDDEF
  | T_ARRAY
  | T_BEGIN of (Types.op)
  | T_DELETE of (Types.op)
  | T_DIM of (Types.op)
  | T_DO
  | T_DONE
  | T_DOWNTO
  | T_ELSE
  | T_END
  | T_FOR of (Types.op)
  | T_IF of (Types.op)
  | T_IN
  | T_LET
  | T_MATCH of (Types.op)
  | T_MUTABLE
  | T_NEW of (Types.op)
  | T_NOT of (Types.op)
  | T_OF
  | T_REC
  | T_REF
  | T_THEN
  | T_TO
  | T_TYPE
  | T_WHILE of (Types.op)
  | T_WITH
  | T_ID of (Types.id_const)
  | T_CID of (Types.cid_const)
  | T_FLOAT of (Types.float_const)
  | T_INT of (Types.int_const)
  | T_CHAR
  | T_CONSTCHAR of (Types.char_const)
  | T_STRING of (Types.string_const)
  | T_FALSE of (Types.bool_const)
  | T_TRUE of (Types.bool_const)
  | T_BOOL
  | T_UNIT
  | T_FLOATST
  | T_INTST
  | T_GIVES
  | T_SEQ
  | T_BAR
  | T_PLUS of (Types.op)
  | T_MINUS of (Types.op)
  | T_TIMES
  | T_DIV
  | T_MOD
  | T_FPLUS of (Types.op)
  | T_FMINUS of (Types.op)
  | T_FTIMES
  | T_FDIV
  | T_POWER
  | T_BANK of (Types.op)
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
  | T_LPAR of (Types.op)
  | T_RPAR
  | T_LBRACK
  | T_RBRACK
  | T_COMA
  | T_COLON

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> AstTypes.ast_stmt list
