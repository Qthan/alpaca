%{
    open Printf
    open Types
    open Ast
%}
%token T_EOF

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

%token<string> T_ID
%token<string> T_CID

%token<float> T_FLOAT
%token<int> T_INT
%token T_CHAR

%token<string> T_CONSTCHAR
%token<string> T_STRING
%token<bool> T_FALSE
%token<bool> T_TRUE
%token T_BOOL
%token T_UNIT
%token T_FLOATST
%token T_INTST

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

%nonassoc T_IN
%nonassoc T_IF
%nonassoc T_THEN
%nonassoc T_ELSE
%left T_SMCOLON T_COMA

%left T_OR 
%left T_AND 

%nonassoc T_SEQ T_NSEQ T_L T_LE T_G T_GE T_EQ T_NEQ T_ASSIGN T_DELETE T_ID T_CID T_ANDDEF

%right T_GIVES
%nonassoc T_REF ARR

%nonassoc T_DIM
%left T_PLUS T_MINUS T_FPLUS T_FMINUS 
%left T_TIMES T_DIV T_MOD T_FTIMES T_FDIV

%right T_POWER

%left UN 

%start program
%type <unit> program
%type <ast_stmt list> stmt_list
%type <ast_stmt> letdef
%type <ast_def list> anddef
%type <ast_def> def
%type <(string * typ) list> parstar
%type <ast_stmt> typedef
%type <(string * ((string * typ list) list)) list> andtdefstar
%type <(string * typ list ) list> constrbar
%type <typ list> typeplus
%type <typ> types 
%type <int> comastar
%type <ast_expr> expr
%type <ast_atom> atom
%type <ast_expr list> comaexpr
%type <ast_atom list> atomstar
%type <count> count
%type <intmb> intmb
%type <ast_clause> clause
%type <ast_clause list> clausestar
%type <ast_pattern> pattern
%type <ast_pattom list> pattomstar
%type <ast_pattom> pattom 
%%

program     : stmt_list T_EOF                                           { walk_program (List.rev($1)) }
            ;

stmt_list: 
            | /* nothing */                                             { [] }
            | stmt_list letdef                                          { $2::$1 } 
            | stmt_list typedef                                         { $2::$1 } 
            ;

letdef:
            | T_LET def anddef                                          { S_Let ($2::(List.rev($3))) }
            | T_LET T_REC def anddef                                    { S_Rec ($3::(List.rev($4))) }
            ;

anddef:
            | /* nothing */                                             { [] }
            | anddef T_ANDDEF def                                       { $3::$1 }
            ;

def:
            | T_ID parstar  T_SEQ expr                                  { D_Var (($1, T_Notype)::(List.rev($2)), $4) }
            | T_ID parstar T_COLON types T_SEQ expr                     { D_Var (($1, $4)::(List.rev($2)), $6) }
            | T_MUTABLE T_ID                                            { D_Mut (($2, T_Notype)) }
            | T_MUTABLE T_ID T_COLON types                              { D_Mut (($2, $4)) }
            | T_MUTABLE T_ID T_LBRACK expr comaexpr T_RBRACK            { D_Arr ($2, T_Notype, ($4::$5)) }
            | T_MUTABLE T_ID T_LBRACK expr comaexpr T_RBRACK T_COLON types  
                                                                        { D_Arr ($2, $8, ($4::$5)) }
            ;

parstar:
            | /* nothing */                                             { [] }
            | parstar T_ID                                              { ($2, T_Notype)::$1 }
            | parstar T_LPAR T_ID T_COLON types T_RPAR                  { ($3, $5)::$1 }
            ;

typedef:
            | T_TYPE T_ID T_SEQ T_CID  constrbar andtdefstar            { S_Type(($2, ($4, [T_Notype])::(List.rev($5)))::(List.rev($6))) }
            | T_TYPE T_ID T_SEQ T_CID T_OF typeplus constrbar andtdefstar
                                                                        { S_Type(($2,($4, (List.rev($6)))::(List.rev($7)))::(List.rev($8))) }
            ;

andtdefstar:
            | /* nothing */                                             { [] }
            | andtdefstar T_ANDDEF T_ID T_SEQ T_CID  constrbar          { ($3, ($5, [T_Notype])::(List.rev($6)))::$1 }
            | andtdefstar T_ANDDEF T_ID T_SEQ T_CID T_OF typeplus constrbar    
                                                                        { ($3, ($5, List.rev($7))::(List.rev($8)))::$1 }
            ;                                                            

constrbar:
            | /* nothing */                                             { [] }
            | constrbar T_BAR T_CID                                     { ($3, [T_Notype])::$1 }
            | constrbar T_BAR T_CID T_OF typeplus                       { ($3, $5)::$1 }
            ;

typeplus:
            | types                                                     { [$1] }
            | typeplus types                                            { $2::$1 }
            ;

types:
            | T_UNIT                                                    { T_Unit }
            | T_INTST                                                   { T_Int }
            | T_CHAR                                                    { T_Chr }
            | T_BOOL                                                    { T_Bool }
            | T_FLOATST                                                 { T_Float }
            | T_LPAR types T_RPAR                                       { $2 }
            | types T_GIVES types                                       { T_Gives ($1, $3) }
            | types T_REF                                               { T_Ref ($1) }
            | T_ARRAY T_OF types %prec ARR                              { T_Arr ($3, 0) }
            | T_ARRAY T_LBRACK T_TIMES comastar T_RBRACK T_OF types %prec ARR                     
                                                                        { T_Arr ($7, $4+1) }
            | T_ID                                                      { T_Id ($1) }
            ;
         
comastar:           
            | /* nothing */                                             { 0 }
            | comastar T_COMA T_TIMES                                   { $1+1 }
            ;

expr:
            | expr T_FPLUS expr                                         { E_Binop ($1, Fplus, $3) } 
            | expr T_PLUS expr                                          { E_Binop ($1, Plus, $3) } 
            | expr T_MINUS expr                                         { E_Binop ($1, Minus, $3) } 
            | expr T_FMINUS expr                                        { E_Binop ($1, Fminus, $3) } 
            | expr T_TIMES expr                                         { E_Binop ($1, Times, $3) } 
            | expr T_FTIMES expr                                        { E_Binop ($1, Ftimes, $3) } 
            | expr T_DIV expr                                           { E_Binop ($1, Div, $3) } 
            | expr T_FDIV expr                                          { E_Binop ($1, Fdiv, $3) }  
            | expr T_MOD expr                                           { E_Binop ($1, Mod, $3) } 
            | expr T_POWER expr                                         { E_Binop ($1, Power, $3) } 
            | expr T_SEQ expr                                           { E_Binop ($1, Seq, $3) }
            | expr T_NSEQ expr                                          { E_Binop ($1, Nseq, $3) } 
            | expr T_L expr                                             { E_Binop ($1, L, $3) } 
            | expr T_G expr                                             { E_Binop ($1, G, $3) }
            | expr T_LE expr                                            { E_Binop ($1, Le, $3) } 
            | expr T_GE expr                                            { E_Binop ($1, Ge, $3) } 
            | expr T_EQ expr                                            { E_Binop ($1, Eq, $3) } 
            | expr T_NEQ expr                                           { E_Binop ($1, Neq, $3) } 
            | expr T_AND expr                                           { E_Binop ($1, And, $3) } 
            | expr T_OR expr                                            { E_Binop ($1, Or, $3) } 
            | expr T_SMCOLON expr                                       { E_Binop ($1, Semicolon, $3) } 
            | expr T_ASSIGN expr                                        { E_Binop ($1, Assign, $3) }  
            | T_PLUS expr %prec UN                                      { E_Unop (U_Plus, $2) } 
            | T_FPLUS expr %prec UN                                     { E_Unop (U_Fplus, $2) } 
            | T_FMINUS expr %prec UN                                    { E_Unop (U_Fminus, $2) }
            | T_MINUS expr %prec UN                                     { E_Unop (U_Minus, $2) }
            | T_NOT expr %prec UN                                       { E_Unop (U_Not, $2 ) }
            | T_DELETE expr %prec UN                                    { E_Unop (U_Del, $2) } 
            | T_BEGIN expr T_END                                        { E_Block ($2) }
            | T_WHILE expr T_DO expr T_DONE                             { E_While ($2, $4) }
            | T_FOR T_ID T_SEQ expr count expr T_DO expr T_DONE         { E_For ($2, $4, $5, $6, $8) }
            | T_DIM intmb T_ID                                          { E_Dim ($2, $3) }
            | T_NEW types                                               { E_New ($2) }
            | T_IF expr T_THEN expr T_ELSE expr                         { E_Ifthenelse ($2, $4, $6) }
            | T_IF expr T_THEN expr                                     { E_Ifthen ($2, $4) }
            | letdef T_IN expr                                          { E_Letin ($1, $3) }
            | T_MATCH expr T_WITH clause clausestar T_END               { E_Match ($2, $4::(List.rev($5))) } 
            | T_CID atomstar                                            { E_Cid ($1, $2) } 
            | T_ID atomstar                                             { E_Id ($1, $2) }
            | atom                                                      { E_Atom ($1) } 
            ;

atom:       
            | T_INT                                                     { A_Num $1 }
            | T_FLOAT                                                   { A_Dec $1 } 
            | T_CONSTCHAR                                               { A_Chr $1 } 
            | T_STRING                                                  { A_Str $1 } 
            | T_TRUE                                                    { A_Bool $1 } 
            | T_FALSE                                                   { A_Bool $1 } 
            | T_LPAR T_RPAR                                             { A_Par } 
        /*  | T_CID                                                     { A_Const $1 } */
            | T_ID                                                      { A_Var $1 } 
            | T_BANK atom                                               { A_Bank $2 }
            | T_ID T_LBRACK expr comaexpr T_RBRACK                      { A_Array ($1, $3::$4) } 
            | T_LPAR expr T_RPAR                                        { A_Expr $2 } 
            ;

comaexpr:  
            |  /* nothing */                                            { [] }  
            | T_COMA expr comaexpr                                      { $2::$3 }
            ;

            
atomstar:  
            | atom                                                      { [$1] } 
            | atom atomstar                                             { $1::$2 } 
            ;

count: 
            | T_TO                                                      { To }
            | T_DOWNTO                                                  { Downto }
            ;

intmb: 
            | /* nothing */                                             { Nonum }
            | T_INT                                                     { Yesnum ($1) } 
            ;
clausestar:
            | /* nothing */                                              { [] }
            | clausestar T_BAR clause                                   { $3::$1 }
            ;

clause:
            | pattern T_GIVES expr                                      { Clause ($1, $3) }
            ;

pattern:
            | pattom                                                    { Pa_Atom $1 }
            | T_CID pattomstar                                          { Pa_Cid ($1, List.rev($2)) }
            ;

pattom:
            | T_PLUS T_INT %prec UN                                     { P_Sign (P_Plus, $2) } 
            | T_MINUS T_INT %prec UN                                    { P_Sign (P_Minus, $2) } 
            | T_FPLUS T_FLOAT %prec UN                                  { P_Fsign (P_Fplus, $2) } 
            | T_FMINUS T_FLOAT %prec UN                                 { P_Fsign (P_Fminus, $2) } 
            | T_INT                                                     { P_Num $1 }
            | T_FLOAT                                                   { P_Float $1 }
            | T_CONSTCHAR                                               { P_Chr $1 }
            | T_TRUE                                                    { P_Bool $1 }
            | T_FALSE                                                   { P_Bool $1 }
            | T_ID                                                      { P_Id $1 }
            | T_CID                                                     { P_Cid $1 }
            | T_LPAR pattern T_RPAR                                     { P_Pattern $2 }
            ;

pattomstar: 
            | pattom                                                    { [$1] }
            | pattomstar pattom                                         { $2::$1 }
            ;
%%
