%{
    open Printf
    open Types
    open Ast
%}
%token T_EOF

%token T_ANDDEF
%token T_ARRAY
%token<op> T_BEGIN
%token<op> T_DELETE
%token<op> T_DIM
%token T_DO
%token T_DONE
%token T_DOWNTO
%token T_ELSE
%token T_END
%token<op> T_FOR
%token<op> T_IF
%token T_IN
%token T_LET
%token<op> T_MATCH
%token T_MUTABLE
%token<op> T_NEW
%token<op> T_NOT
%token T_OF
%token T_REC
%token T_REF
%token T_THEN
%token T_TO
%token T_TYPE
%token<op> T_WHILE
%token T_WITH

%token<id_const> T_ID
%token<cid_const> T_CID

%token<float_const> T_FLOAT
%token<int_const> T_INT
%token T_CHAR

%token<char_const> T_CONSTCHAR
%token<string_const> T_STRING
%token<bool_const> T_FALSE
%token<bool_const> T_TRUE
%token T_BOOL
%token T_UNIT
%token T_FLOATST
%token T_INTST

%token T_GIVES
%token T_SEQ 
%token T_BAR
%token<op> T_PLUS 
%token<op> T_MINUS
%token T_TIMES
%token T_DIV
%token T_MOD
%token<op> T_FPLUS
%token<op> T_FMINUS
%token T_FTIMES
%token T_FDIV
%token T_POWER
%token<op> T_BANK
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

%token<op> T_LPAR
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
%type <ast_def_node list> anddef
%type <ast_def_node> def
%type <(string * typ) list> parstar
%type <ast_stmt> typedef
%type <(string * ((string * typ list) list)) list> andtdefstar
%type <(string * typ list ) list> constrbar
%type <typ list> typeplus
%type <typ> types 
%type <int> comastar
%type <ast_expr_node> expr
%type <ast_atom_node> atom
%type <ast_expr_node list> comaexpr
%type <ast_atom_node list> atomstar
%type <count> count
%type <intmb> intmb
%type <ast_clause> clause
%type <ast_clause list> clausestar
%type <ast_pattern_node> pattern
%type <ast_pattom_node list> pattomstar
%type <ast_pattom_node> pattom 
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
            | T_ID parstar  T_SEQ expr                                  { { def = D_Var (($1.id_name, T_Notype) :: (List.rev($2), $4)); pos = $1.pos; entry = None } }
            | T_ID parstar T_COLON types T_SEQ expr                     { { def = D_Var (($1.id_name, $4) :: (List.rev($2), $6)); pos = $1.pos; entry = None } }
            | T_MUTABLE T_ID                                            { { def = D_Mut ($2.id_name, T_Notype); pos = $2.pos; entry = None } }
            | T_MUTABLE T_ID T_COLON types                              { { def = D_Mut ($2.id_name. $4); pos = $2.pos; entry = None } }
            | T_MUTABLE T_ID T_LBRACK expr comaexpr T_RBRACK            { { def = D_Array (($2.id_name, T_Notype), ($4::$5)); pos = $2.pos; entry = None } }
            | T_MUTABLE T_ID T_LBRACK expr comaexpr T_RBRACK T_COLON types  
                                                                        { { def = D_Array (($2.id_name, T_Notype), ($4::$5)); pos = $2.pos; entry = None } }
            ;

parstar:
            | /* nothing */                                             { [] }
            | parstar T_ID                                              { ($2.id_name, T_Notype)::$1 }
            | parstar T_LPAR T_ID T_COLON types T_RPAR                  { ($3.id_name, $5)::$1 }
            ;

typedef:
            | T_TYPE T_ID T_SEQ T_CID  constrbar andtdefstar            { S_Type(($2, ($4, [])::(List.rev($5)))::(List.rev($6))) }
            | T_TYPE T_ID T_SEQ T_CID T_OF typeplus constrbar andtdefstar
                                                                        { S_Type(($2,($4, (List.rev($6)))::(List.rev($7)))::(List.rev($8))) }
            ;

andtdefstar:
            | /* nothing */                                             { [] }
            | andtdefstar T_ANDDEF T_ID T_SEQ T_CID  constrbar          { ($3, ($5, [])::(List.rev($6)))::$1 }
            | andtdefstar T_ANDDEF T_ID T_SEQ T_CID T_OF typeplus constrbar    
                                                                        { ($3, ($5, List.rev($7))::(List.rev($8)))::$1 }
            ;                                                            

constrbar:
            | /* nothing */                                             { [] }
            | constrbar T_BAR T_CID                                     { ($3, [])::$1 }
            | constrbar T_BAR T_CID T_OF typeplus                       { ($3, List.rev($5))::$1 }
            ;

typeplus:
            | types                                                     { [$1] }
            | typeplus types                                            { $2::$1 }
            ;

types:
            | T_UNIT                                                    { T_Unit }
            | T_INTST                                                   { T_Int }
            | T_CHAR                                                    { T_Char }
            | T_BOOL                                                    { T_Bool }
            | T_FLOATST                                                 { T_Float }
            | T_LPAR types T_RPAR                                       { $2 }
            | types T_GIVES types                                       { T_Arrow ($1, $3) }
            | types T_REF                                               { T_Ref ($1) }
            | T_ARRAY T_OF types %prec ARR                              { T_Array ($3, 0) }
            | T_ARRAY T_LBRACK T_TIMES comastar T_RBRACK T_OF types %prec ARR                     
                                                                        { T_Array ($7, $4+1) }
            | T_ID                                                      { T_Id ($1) }
            ;
         
comastar:           
            | /* nothing */                                             { 0 }
            | comastar T_COMA T_TIMES                                   { $1+1 }
            ;

expr:
            | expr T_FPLUS expr                                         { { expr = E_Binop ($1, Fplus, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_PLUS expr                                          { { expr = E_Binop ($1, Plus, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_MINUS expr                                         { { expr = E_Binop ($1, Minus, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_FMINUS expr                                        { { expr = E_Binop ($1, Fminus, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_TIMES expr                                         { { expr = E_Binop ($1, Times, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_FTIMES expr                                        { { expr = E_Binop ($1, Ftimes, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } }  
            | expr T_DIV expr                                           { { expr = E_Binop ($1, Div, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_FDIV expr                                          { { expr = E_Binop ($1, Fdiv, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } }   
            | expr T_MOD expr                                           { { expr = E_Binop ($1, Mod, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_POWER expr                                         { { expr = E_Binop ($1, Power, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_SEQ expr                                           { { expr = E_Binop ($1, Seq, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_NSEQ expr                                          { { expr = E_Binop ($1, Nseq, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_L expr                                             { { expr = E_Binop ($1, L, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_G expr                                             { { expr = E_Binop ($1, G, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_LE expr                                            { { expr = E_Binop ($1, Le, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_GE expr                                            { { expr = E_Binop ($1, Ge, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_EQ expr                                            { { expr = E_Binop ($1, Eq, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_NEQ expr                                           { { expr = E_Binop ($1, Neq, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_AND expr                                           { { expr = E_Binop ($1, And, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_OR expr                                            { { expr = E_Binop ($1, Or, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } }
            | expr T_SMCOLON expr                                       { { expr = E_Binop ($1, Semicolon, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | expr T_ASSIGN expr                                        { { expr = E_Binop ($1, Assing, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } }  
            | T_PLUS expr %prec UN                                      { { expr = E_Unop (U_Plus, $2); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | T_FPLUS expr %prec UN                                     { { expr = E_Unop (U_Fplus, $2); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | T_FMINUS expr %prec UN                                    { { expr = E_Unop (U_Fminus, $2); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | T_MINUS expr %prec UN                                     { { expr = E_Unop (U_Minus, $2); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | T_NOT expr %prec UN                                       { { expr = E_Unop (U_Not, $2); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | T_DELETE expr %prec UN                                    { { expr = E_Unop (U_Del, $2); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | T_BEGIN expr T_END                                        { { expr = E_Block ($2); pos = $1.pos; expr_typ = T_Notype; entry = None } }
            | T_WHILE expr T_DO expr T_DONE                             { { expr = E_While ($2, $4); pos = $1.pos; expr_typ = T_Notype; entry = None } }
            | T_FOR T_ID T_SEQ expr count expr T_DO expr T_DONE         { { expr = E_For ($2, $4, $5, $6, $8); pos = $1.pos; expr_typ = T_Notype; entry = None } }
            | T_DIM intmb T_ID                                          { { expr = E_Dim ($2, $3); pos = $1.pos; expr_typ = T_Notype; entry = None } }
            | T_NEW types                                               { { expr = E_New ($2); pos = $1.pos; expr_typ = T_Notype; entry = None } }
            | T_IF expr T_THEN expr T_ELSE expr                         { { expr = E_Ifthenelse ($2, $4, $6); pos = $1.pos; expr_typ = T_Notype; entry = None } }
            | T_IF expr T_THEN expr                                     { { expr = E_Ifthen ($2, $4); pos = $1.pos; expr_typ = T_Notype; entry = None } }
            | letdef T_IN expr                                          { { expr = E_Letin ($1, $3); pos = $3.pos; expr_typ = T_Notype; entry = None } }
            | T_MATCH expr T_WITH clause clausestar T_END               { { expr = E_Match ($2, $4::(List.rev($5))); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | T_CID atomstar                                            { { expr = E_Cid ($1.cid_name, $2); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            | T_ID atomstar                                             { { expr = E_Id ($1.id_name, $2); pos = $1.pos; expr_typ = T_Notype; entry = None } }
            | atom                                                      { { expr = E_Atom ($1); pos = $1.pos; expr_typ = T_Notype; entry = None } } 
            ;

atom:       
            | T_INT                                                     { { atom = A_Num $1.ival; pos = $1.pos; atom_typ = T_Notype; entry = None } }
            | T_FLOAT                                                   { { atom = A_Dec $1.fval; pos = $1.pos; atom_typ = T_Notype; entry = None } }
            | T_CONSTCHAR                                               { { atom = A_Chr $1.cval; pos = $1.pos; atom_typ = T_Notype; entry = None } }
            | T_STRING                                                  { { atom = A_Str $1.sval; pos = $1.pos; atom_typ = T_Notype; entry = None } } 
            | T_TRUE                                                    { { atom = A_Bool $1.bval; pos = $1.pos; atom_typ = T_Notype; entry = None } } 
            | T_FALSE                                                   { { atom = A_Bool $1.bval; pos = $1.pos; atom_typ = T_Notype; entry = None } } 
            | T_LPAR T_RPAR                                             { { atom = A_Par; pos = $1.pos; atom_typ = T_Notype; entry = None } } 
            | T_CID                                                     { { atom = A_Cid $1.cid_name; pos = $1.pos; atom_typ = T_Notype; entry = None } } 
            | T_ID                                                      { { atom = A_Var $1.id_name; pos = $1.pos; atom_typ = T_Notype; entry = None } } 
            | T_BANK atom                                               { { atom = A_Bank $2; pos = $1.pos; atom_typ = T_Notype; entry = None } }
            | T_ID T_LBRACK expr comaexpr T_RBRACK                      { { atom = A_Array ($1.id_name, $3::$4); pos = $1.pos; atom_typ = T_Notype; entry = None } }
            | T_LPAR expr T_RPAR                                        { { atom = A_Expr $2; pos = $1.pos; atom_typ = T_Notype; entry = None } } 
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
            | pattom                                                    { { pattern = Pa_Atom $1; pos = $1.pos; pattern_typ = T_Notype; entry = None } }
            | T_CID pattomstar                                          { { pattern = Pa_Cid ($1.cid_name, List.rev($2)); pos = $1.pos; pattern_typ = T_Notype; entry = None } }
            ;

pattom:
            | T_PLUS T_INT %prec UN                                     { { pattom = P_Sign (P_Plus, $2.ival); pos = $1.pos; pattom_typ = T_Notype; entry = None } } 
            | T_MINUS T_INT %prec UN                                    { { pattom = P_Sign (P_Minus, $2.ival); pos = $1.pos; pattom_typ = T_Notype; entry = None } } 
            | T_FPLUS T_FLOAT %prec UN                                  { { pattom = P_Fsign (P_Fplus, $2.fval); pos = $1.pos; pattom_typ = T_Notype; entry = None } } 
            | T_FMINUS T_FLOAT %prec UN                                 { { pattom = P_Fsign (P_Fminus, $2.fval); pos = $1.pos; pattom_typ = T_Notype; entry = None } } 
            | T_INT                                                     { { pattom = P_Num $1.ival; pos = $1.pos; pattom_typ = T_Notype; entry = None } }
            | T_FLOAT                                                   { { pattom = P_Float $1.fval; pos = $1.pos; pattom_typ = T_Notype; entry = None } }
            | T_CONSTCHAR                                               { { pattom = P_Chr $1.cval; pos = $1.pos; pattom_typ = T_Notype; entry = None } }
            | T_TRUE                                                    { { pattom = P_Bool $1.bval; pos = $1.pos; pattom_typ = T_Notype; entry = None } }
            | T_FALSE                                                   { { pattom = P_Bool $1.bval; pos = $1.pos; pattom_typ = T_Notype; entry = None } }
            | T_ID                                                      { { pattom = P_Id $1.id_name; pos = $1.pos; pattom_typ = T_Notype; entry = None } }
            | T_CID                                                     { { pattom = P_Cid $1.cid_name; pos = $1.pos; pattom_typ = T_Notype; entry = None } }
            | T_LPAR pattern T_RPAR                                     { { pattom = P_Pattern $2; pos = $1.pos; pattom_typ = T_Notype; entry = None } }
            ;

pattomstar: 
            | pattom                                                    { [$1] }
            | pattomstar pattom                                         { $2::$1 }
            ;
%%

