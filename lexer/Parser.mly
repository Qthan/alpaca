%token T_ANDDEF
%token T_ARRAY
%token T_BEGIN
%token T_DELETE
%token T_DIM
%token T_DO
%token T_DONE
%token T_DOWNTO
%token T_ELSE
%token T_END
%token T_FOR
%token T_IF
%token T_IN
%token T_LET
%token T_MATCH
%token T_MUTABLE
%token T_NEW
%token T_NOT
%token T_OF
%token T_REC
%token T_REF
%token T_THEN
%token T_TO
%token T_TYPE
%token T_WHILE
%token T_WITH

%token T_ID 
%token T_CID 

%token T_FLOAT
%token T_INT

%token T_CHAR
%token T_STRING
%token T_FALSE
%token T_TRUE
%token T_BOOL
%token T_UNIT

%token T_GIVES
%token T_SEQ 
%token T_BAR
%token T_PLUS 
%token T_MINUS
%token T_TIMES
%token T_DIV
%token T_MOD
%token T_FPLUS
%token T_FMINUS
%token T_FTIMES
%token T_FDIV
%token T_POWER
%token T_BANK
%token T_SMCOLON
%token T_AND
%token T_OR
%token T_NSEQ
%token T_L
%token T_G
%token T_LE
%token T_GE
%token T_EQ
%token T_NEQ
%token T_ASSIGN

%token T_LPAR
%token T_RPAR
%token T_LBRACK
%token T_RBRACK
%token T_COMA
%token T_COLON

%token T_EOF

%left T_SMCOLON
%left T_AND T_OR 
%left T_PLUS T_MINUS T_FPLUS T_FMINUS 
%left T_TIMES T_DIV T_MOD T_FTIMES T_FDIV

%right T_POWER

%nonassoc T_NEW T_SEQ T_NSEQ T_L T_LE T_G T_GE T_EQ T_NEQ T_ASSIGN

%start program
%type <unit> program

%%

program     : stmt_list T_EOF { () }

stmt_list   : /* nothing */ { () }
            | stmt_list letdef { () }
            | stmt_list typedef { () }
            
letdef      : T_LET recz def anddef { () } 

recz        :  /* nothing */ { () }
            | T_REC { () }

anddef      : /* nothing */ { () }
            | anddef T_ANDDEF def { () } 

def         : T_ID parstar  colon T_SEQ expr { () }
            | T_MUTABLE T_ID arrayz colon { () } 

colon       : /* nothing */ { () }
            | T_COLON typez { () } 

arrayz      : /* nothing */ { () } 
            | T_LBRACK expr coma T_RBRACK { () } 

coma        : /* nothing */ { () } 
            | coma T_COMA expr { () } 

typedef     : T_TYPE tdef andtdef { () } 

andtdef     :  /* nothing */ { () }
            | andtdef T_ANDDEF tdef { () } 

tdef        : T_ID T_SEQ constr barconstr { () } 

barconstr   : /* nothing */ { () } 
            | barconstr T_BAR constr { () } 

constr      : T_ID oftype { () } 

oftype      : /* nothing */ { () } 
            | T_OF typepl { () } 

typepl      : typez { () } 
            | typepl typez { () } 

parstar     : /* nothing */ { () } 
            | parstar par { () } 

par         : T_ID { () } 
            | T_LPAR T_ID T_COLON typez T_RPAR { () } 

typez       : T_UNIT { () } 
            | T_INT { () } 
            | T_CHAR { () } 
            | T_BOOL { () } 
            | T_FLOAT { () } 
            | T_LPAR typez T_RPAR { () } 
            | typez T_GIVES typez { () } 
            | typez T_REF { () } 
            | T_ARRAY T_LBRACK T_TIMES arrbrack T_RBRACK T_OF typez { () } 
            | T_ID { () } 

arrbrack    : /* nothing */ { () } 
            | arrbrack T_COMA T_TIMES  { () } 

expr        : T_INT { () } 
            | T_FLOAT { () } 
            | T_CHAR  { () } 
            | T_STRING  { () } 
            | T_TRUE { () } 
            | T_FALSE { () } 
            | T_LPAR T_RPAR { () } 
            | T_LPAR expr T_RPAR { () } 
            | unop expr { () } 
            | expr binop expr { () } 
            | T_CID exprstar { () } 
            | T_ID exprstar { () } 
            | T_ID T_LBRACK expr comaexpr T_RBRACK  { () } 
            | T_DIM T_INT T_ID { () } 
            | T_NEW typez { () } 
            | T_DELETE expr { () } 
            | letdef T_IN expr { () } 
            | T_BEGIN expr T_END { () } 
            | T_IF expr T_THEN expr elsexpr  { () } 
            | T_WHILE expr T_DO expr T_DONE { () } 
            | T_MATCH  expr T_WITH clause barclause T_END { () } 

elsexpr     : /* nothing */ { () } 
            | T_ELSE expr { () } 

exprstar    :  /* nothing */ { () }
            | exprstar expr { () } 

comaexpr    : /* nothing */ { () } 
            | comaexpr T_COMA expr { () } 

barclause   : /* nothing */ { () } 
            | barclause T_BAR clause { () } 

unop        : T_PLUS { () } 
            | T_MINUS { () } 
            | T_FPLUS { () } 
            | T_FMINUS { () } 
            | T_BANK { () } 
            | T_NOT { () } 

binop       : T_PLUS { () } 
            | T_FPLUS { () } 
            | T_MINUS { () } 
            | T_FMINUS { () } 
            | T_TIMES { () } 
            | T_FTIMES { () } 
            | T_DIV { () } 
            | T_FDIV { () } 
            | T_MOD { () } 
            | T_POWER { () } 
            | T_SEQ { () } 
            | T_NSEQ { () } 
            | T_L { () } 
            | T_G { () } 
            | T_LE { () } 
            | T_GE { () } 
            | T_EQ { () } 
            | T_NEQ { () } 
            | T_AND { () } 
            | T_OR { () } 
            | T_SMCOLON { () } 
            | T_ASSIGN { () } 

clause      : pattern T_GIVES expr { () } 

pattern     : T_PLUS T_INT { () } 
            | T_MINUS T_INT { () } 
            | T_FPLUS T_FLOAT { () } 
            | T_FMINUS T_FLOAT { () } 
            | T_CHAR { () } 
            | T_TRUE { () } 
            | T_FALSE { () } 
            | T_ID { () } 
            | T_LPAR pattern T_RPAR { () } 
            | T_CID patternstar { () } 

patternstar : /* nothing */ { () } 
            | patternstar patternstar { () } 

%%
