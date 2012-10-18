%{
    open Ast
    open Printf
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
%token<char> T_CONSTCHAR
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
%type<unit> program
%type<ast_atom> atom
%type<ast_expr> expr
%type<ast_expr list> comaexpr
%type<count> count
%type<intmb> intmb
%type<ast_atom list> atomstar
%type<ast_clause> clause
%type<ast_clause list> clausestar
%type<ast_pattern> pattern
%type<ast_pattom list> pattomstar
%type<ast_pattom> pattom 
%type<ast_def> def
%type<(string * ast_types) list> parstar
%type<ast_types>types 
%type<int>comastar
%type<ast_def list>anddef
%type<ast_letdef> letdef
%type<ast_types list>typeplus
%type<( string*ast_types list ) list>constrbar
%type<(string * ( ( string*ast_types list ) list) ) list>andtdefstar
%type<ast_typedef>typedef
%%

program     : stmt_list T_EOF                                           { () }
            ;

stmt_list: 
            | /* nothing */                                             { () }
            | stmt_list letdef                                          { printf"%a\n" pp_letdef $2 } 
            | stmt_list typedef                                         { printf"%a\n" pp_typedef $2 } 
            ;

letdef:
            | T_LET def anddef                                          { L_Let($2::$3) }
            | T_LET T_REC def anddef                                    { L_Rec($3::$4) }
            ;

anddef:
            | /* nothing */                                             { [] }
            | anddef T_ANDDEF def                                       { $3::$1 }
            ;

def:
            | T_ID parstar  T_SEQ expr                                  { D_var(($1,T_notype)::$2, $4) }
            | T_ID parstar T_COLON types T_SEQ expr                     { D_var(($1,$4)::$2, $6) }
            | T_MUTABLE T_ID                                            { D_mut(($2,T_notype)) }
            | T_MUTABLE T_ID T_COLON types                              { D_mut(($2,$4)) }
            | T_MUTABLE T_ID T_LBRACK expr comaexpr T_RBRACK            { D_arr($2,T_notype, ($4::$5)) }
            | T_MUTABLE T_ID T_LBRACK expr comaexpr T_RBRACK T_COLON types  
                                                                        { D_arr($2,$8, ($4::$5)) }
            ;

parstar:
            | /* nothing */                                             { [] }
            | parstar T_ID                                              { ($2, T_notype)::$1 }
            | parstar T_LPAR T_ID T_COLON types T_RPAR                  { ($3, $5)::$1 }
            ;

typedef:
            | T_TYPE T_ID T_SEQ T_CID constrbar andtdefstar            { T_Typedef(($2, ($4, [T_notype])::$5)::$6) }
            | T_TYPE T_ID T_SEQ T_CID T_OF typeplus constrbar andtdefstar
                                                                        { T_Typedef(($2, ($4, $6)::$7)::$8) }
            ;

andtdefstar:
            | /* nothing */                                             { [] }
            | andtdefstar T_ANDDEF T_ID T_SEQ T_CID  constrbar          { ($3,($5, [T_notype])::$6)::$1 }
            | andtdefstar T_ANDDEF T_ID T_SEQ T_CID T_OF typeplus constrbar    
                                                                        { ($3,($5,$7)::$8)::$1 }
            ;                                                            

constrbar:
            | /* nothing */                                             { [] }
            | constrbar T_BAR T_CID                                     { ($3, [T_notype])::$1 }
            | constrbar T_BAR T_CID T_OF typeplus                       { ($3, $5)::$1 }
            ;

typeplus:
            | types                                                     { [$1] }
            | typeplus types                                            { $2::$1 }
            ;

types:
            | T_UNIT                                                    { T_unit }
            | T_INTST                                                   { T_int }
            | T_CHAR                                                    { T_chr }
            | T_BOOL                                                    { T_bool }
            | T_FLOATST                                                 { T_float }
            | T_LPAR types T_RPAR                                       { $2 }
            | types T_GIVES types                                       { T_gives($1, $3) }
            | types T_REF                                               { T_ref($1) }
            | T_ARRAY T_OF types %prec ARR                              { T_arr($3,0) }
            | T_ARRAY T_LBRACK T_TIMES comastar T_RBRACK T_OF types %prec ARR                     
                                                                        { T_arr($7, $4+1) }
            | T_ID                                                      { T_id($1) }
            ;
         
comastar:           
            | /* nothing */                                             { 0 }
            | comastar T_COMA T_TIMES                                   { $1+1 }
            
expr:
            | expr T_FPLUS expr                                         { Binop ($1, Fplus, $3) } 
            | expr T_PLUS expr                                          { Binop ($1, Plus, $3) } 
            | expr T_MINUS expr                                         { Binop ($1, Minus, $3) } 
            | expr T_FMINUS expr                                        { Binop ($1, Fminus, $3) } 
            | expr T_TIMES expr                                         { Binop ($1, Times, $3) } 
            | expr T_FTIMES expr                                        { Binop ($1, Ftimes, $3) } 
            | expr T_DIV expr                                           { Binop ($1, Div, $3) } 
            | expr T_FDIV expr                                          { Binop ($1, Fdiv, $3) }  
            | expr T_MOD expr                                           { Binop ($1, Mod, $3) } 
            | expr T_POWER expr                                         { Binop ($1, Power, $3) } 
            | expr T_SEQ expr                                           { Binop ($1, Seq, $3) }
            | expr T_NSEQ expr                                          { Binop ($1, Nseq, $3) } 
            | expr T_L expr                                             { Binop ($1, L, $3) } 
            | expr T_G expr                                             { Binop ($1, G, $3) } 
            | expr T_LE expr                                            { Binop ($1, Le, $3) } 
            | expr T_GE expr                                            { Binop ($1, Ge, $3) } 
            | expr T_EQ expr                                            { Binop ($1, Eq, $3) } 
            | expr T_NEQ expr                                           { Binop ($1, Neq, $3) } 
            | expr T_AND expr                                           { Binop ($1, And, $3) } 
            | expr T_OR expr                                            { Binop ($1, Or, $3) } 
            | expr T_SMCOLON expr                                       { Binop ($1, Semicolon, $3) } 
            | expr T_ASSIGN expr                                        { Binop ($1, Assign, $3) }  
            | T_PLUS expr %prec UN                                      { Unop (U_Plus, $2) } 
            | T_FPLUS expr %prec UN                                     { Unop (U_Fplus, $2) } 
            | T_FMINUS expr %prec UN                                    { Unop (U_Fminus, $2) } 
            | T_MINUS expr %prec UN                                     { Unop (U_Minus, $2) } 
            | T_NOT expr %prec UN                                       { Unop (U_Not, $2 ) }
            | T_DELETE expr %prec UN                                    { Unop (U_Del, $2) } 
            | T_BEGIN expr T_END                                        { Block ($2) }
            | T_WHILE expr T_DO expr T_DONE                             { While ($2, $4) }
            | T_FOR T_ID T_SEQ expr count expr T_DO expr T_DONE         { For ($2, $4, $5, $6, $8) }
            | T_DIM intmb T_ID                                          { Dim ($2, $3) }
            /* | T_NEW types                                               { () } */
            | T_IF expr T_THEN expr T_ELSE expr                         { Ifthelse ($2, $4, $6) }
            | T_IF expr T_THEN expr                                     { Ifthe($2, $4) }
            /* | letdef T_IN expr                                          { () } */
            | T_MATCH expr T_WITH clause clausestar T_END               { E_Match ($2, $4::$5) } 
            | T_CID atomstar                                            { E_Cid ($1, $2) } 
            | T_ID atomstar                                             { E_Id ($1, $2) }
            | atom                                                      { Atom $1 }            
            ;

atom:       
            | T_INT                                                     { Num $1 } 
            | T_FLOAT                                                   { Dec $1 } 
            | T_CONSTCHAR                                               { Chr $1 } 
            | T_STRING                                                  { Str $1 } 
            | T_TRUE                                                    { Bool $1 } 
            | T_FALSE                                                   { Bool $1 } 
            | T_LPAR T_RPAR                                             { Par } 
            | T_CID                                                     { Const $1 } 
            | T_ID                                                      { Var $1 } 
            | T_BANK atom                                               { Bank $2 }
            | T_ID T_LBRACK expr comaexpr T_RBRACK                      { Array($1, $3::$4) } 
            | T_LPAR expr T_RPAR                                        { Expr $2 } 
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
            | /*nathing */                                              { [] }
            | clausestar T_BAR clause                                   { $3::$1 }
            ;

clause:
            | pattern T_GIVES expr                                      { Clause ($1, $3) }
            ;

pattern:
            | pattom                                                    { Pa_Atom $1 }
            | T_CID pattomstar                                          { Pa_Cid ($1, $2) }
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
