%{
    open Printf
    open Error
    open Types
    open AstTypes
    open Ast

let def_expr =
  {
    expr = E_None;
    expr_pos = (0, 0);
    expr_typ = T_Notype;
    expr_entry = None;
    expr_tail = false
  }

let def_atom =
  {
    atom = A_None;
    atom_pos = (0, 0);
    atom_typ = T_Notype;
    atom_entry = None;
    atom_tail = false
  }

%}
%token T_EOF

%token T_ANDDEF
%token T_ARRAY
%token<Types.op> T_BEGIN
%token<Types.op> T_DELETE
%token<Types.op> T_DIM
%token T_DO
%token T_DONE
%token T_DOWNTO
%token T_ELSE
%token T_END
%token<Types.op> T_FOR
%token<Types.op> T_IF
%token T_IN
%token T_LET
%token<Types.op> T_MATCH
%token T_MUTABLE
%token<Types.op> T_NEW
%token<Types.op> T_NOT
%token T_OF
%token T_REC
%token T_REF
%token T_THEN
%token T_TO
%token T_TYPE
%token<Types.op> T_WHILE
%token T_WITH

%token<Types.id_const> T_ID
%token<Types.cid_const> T_CID

%token<Types.float_const> T_FLOAT
%token<Types.int_const> T_INT
%token T_CHAR

%token<Types.char_const> T_CONSTCHAR
%token<Types.string_const> T_STRING
%token<Types.bool_const> T_FALSE
%token<Types.bool_const> T_TRUE
%token T_BOOL
%token T_UNIT
%token T_FLOATST
%token T_INTST

%token T_GIVES
%token T_SEQ 
%token T_BAR
%token<Types.op> T_PLUS 
%token<Types.op> T_MINUS
%token T_TIMES
%token T_DIV
%token T_MOD
%token<Types.op> T_FPLUS
%token<Types.op>  T_FMINUS
%token T_FTIMES
%token T_FDIV
%token T_POWER
%token<Types.op>  T_BANK
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

%token<Types.op>  T_LPAR
%token T_RPAR
%token T_LBRACK
%token T_RBRACK
%token T_COMA
%token T_COLON


%nonassoc T_IN
%left T_SMCOLON T_COMA
%nonassoc IFX
%nonassoc T_ELSE
%nonassoc T_ASSIGN
%left T_OR 
%left T_AND 
%nonassoc T_SEQ T_NSEQ T_L T_LE T_G T_GE T_EQ T_NEQ
%left T_PLUS T_MINUS T_FPLUS T_FMINUS
%left T_TIMES T_DIV T_MOD T_FTIMES T_FDIV
%right T_POWER
%left UN
%nonassoc T_BANK
%nonassoc T_NEW T_LBRACK T_RBRACK T_MUTABLE 


%right T_GIVES
%left T_REF
%nonassoc T_ARRAY ARR





%start program
%type <AstTypes.ast_stmt list> program
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
%type <ast_clause> clause
%type <ast_clause list> clausestar
%type <ast_pattern_node> pattern
%type <ast_pattom_node list> pattomstar
%type <ast_pattom_node> pattom 
%%

program     : stmt_list T_EOF                                           
              { List.rev $1 }
            ;

stmt_list: 
            | /* nothing */                                             
              { [] }
            | stmt_list letdef                                          
              { $2::$1 } 
            | stmt_list typedef                                         
              { $2::$1 } 
            ;

letdef:
            | T_LET def anddef                                          
              { S_Let ($2::(List.rev($3))) }
            | T_LET T_REC def anddef                                    
              { S_Rec ($3::(List.rev($4))) }
            ;

anddef:
            | /* nothing */                                             
              { [] }
            | anddef T_ANDDEF def                                       
              { $3::$1 }
            ;

def:
            | T_ID parstar  T_SEQ expr                                  
              { { 
                  def = D_Var ((($1.id_name, T_Notype) :: (List.rev($2))), $4); 
                  def_pos = $1.id_pos; def_entry = None 
              } }
            | T_ID parstar T_COLON types T_SEQ expr                     
              { { 
                  def = D_Var ((($1.id_name, $4) :: (List.rev($2))), $6); 
                  def_pos = $1.id_pos; def_entry = None 
              } }
            | T_MUTABLE T_ID                                            
              { { 
                  def = D_Mut ($2.id_name, T_Notype); 
                  def_pos = $2.id_pos; def_entry = None 
              } }
            | T_MUTABLE T_ID T_COLON types                              
              { { 
                  def = D_Mut ($2.id_name, $4); 
                  def_pos = $2.id_pos; def_entry = None 
              } }
            | T_MUTABLE T_ID T_LBRACK expr comaexpr T_RBRACK            
              { { 
                  def = D_Array ($2.id_name, T_Notype, ($4::$5)); 
                  def_pos = $2.id_pos; def_entry = None 
              } }
            | T_MUTABLE T_ID T_LBRACK expr comaexpr T_RBRACK T_COLON types  
              { { 
                  def = D_Array ($2.id_name, $8, ($4::$5)); 
                  def_pos = $2.id_pos; def_entry = None 
              } }
            ;

parstar:
            | /* nothing */                                             
              { [] }
            | parstar T_ID                                              
              { ($2.id_name, T_Notype)::$1 }
            | parstar T_LPAR T_ID T_COLON types T_RPAR                  
              { ($3.id_name, $5)::$1 }
            ;

typedef:
            | T_TYPE T_ID T_SEQ T_CID  constrbar andtdefstar            
              { S_Type (($2.id_name, 
                        ($4.cid_name, [])::(List.rev($5)))::(List.rev($6))) }
            | T_TYPE T_ID T_SEQ T_CID T_OF typeplus constrbar andtdefstar
              { S_Type (($2.id_name,($4.cid_name, 
                      (List.rev($6)))::(List.rev($7)))::(List.rev($8))) }
            ;

andtdefstar:
            | /* nothing */                                             
              { [] }
            | andtdefstar T_ANDDEF T_ID T_SEQ T_CID  constrbar          
              { ($3.id_name, ($5.cid_name, [])::(List.rev($6)))::$1 }
            | andtdefstar T_ANDDEF T_ID T_SEQ T_CID T_OF typeplus constrbar    
              { ($3.id_name, ($5.cid_name, List.rev($7))::(List.rev($8)))::$1 }
            ;                                                            

constrbar:
            | /* nothing */                                             
              { [] }
            | constrbar T_BAR T_CID                                     
              { ($3.cid_name, [])::$1 }
            | constrbar T_BAR T_CID T_OF typeplus                       
              { ($3.cid_name, List.rev($5))::$1 }
            ;

typeplus:
            | types                                                     
              { [$1] }
            | typeplus types                                            
              { $2::$1 }
            ;

types:
            | T_UNIT                                                    
              { T_Unit }
            | T_INTST                                                   
              { T_Int }
            | T_CHAR                                                    
              { T_Char }
            | T_BOOL                                    
              { T_Bool }
            | T_FLOATST                                                 
              { T_Float }
            | T_LPAR types T_RPAR                                       
              { $2 }
            | types T_GIVES types                                       
              { T_Arrow ($1, $3) }
            | types T_REF                                               
              { T_Ref ($1) }
            | T_ARRAY T_OF types %prec ARR                              
              { T_Array ($3, D_Int 0) }
            | T_ARRAY T_LBRACK T_TIMES comastar T_RBRACK T_OF types %prec ARR                    
              { T_Array ($7, D_Int ($4+1)) }
            | T_ID                                                      
              { T_Id ($1.id_name) }
            ;
comastar:           
            | /* nothing */                                             
              { 0 }
            | comastar T_COMA T_TIMES                                   
              { $1+1 }
            ;

expr:
            | expr T_FPLUS expr                                         
              { { 
                  def_expr with 
                  expr = E_Binop ($1, Fplus, $3); 
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_PLUS expr                                          
              { { 
                  def_expr with expr = E_Binop ($1, Plus, $3);
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_MINUS expr                                         
              { { 
                  def_expr with expr = E_Binop ($1, Minus, $3); 
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_FMINUS expr                                        
              { { 
                  def_expr with expr = E_Binop ($1, Fminus, $3); 
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_TIMES expr                                         
              { { 
                  def_expr with expr = E_Binop ($1, Times, $3); 
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_FTIMES expr                                        
              { { 
                  def_expr with expr = E_Binop ($1, Ftimes, $3); 
                  expr_pos = $1.expr_pos 
              } }  
            | expr T_DIV expr                                           
              { { 
                  def_expr with expr = E_Binop ($1, Div, $3); 
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_FDIV expr                                          
              { { 
                  def_expr with expr = E_Binop ($1, Fdiv, $3); 
                  expr_pos = $1.expr_pos 
              } }   
            | expr T_MOD expr                                           
              { { 
                  def_expr with expr = E_Binop ($1, Mod, $3); 
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_POWER expr                                         
              { { 
                  def_expr with expr = E_Binop ($1, Power, $3); 
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_SEQ expr                                           
              { { 
                  def_expr with expr = E_Binop ($1, Seq, $3); 
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_NSEQ expr                                          
              { { 
                  def_expr with expr = E_Binop ($1, Nseq, $3); 
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_L expr                                             
              { { 
                  def_expr with expr = E_Binop ($1, L, $3); 
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_G expr                                             
              { { 
                  def_expr with expr = E_Binop ($1, G, $3); 
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_LE expr                                            
              { { 
                  def_expr with expr = E_Binop ($1, Le, $3); 
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_GE expr                                            
              { { 
                  def_expr with expr = E_Binop ($1, Ge, $3); 
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_EQ expr                                            
              { { 
                  def_expr with expr = E_Binop ($1, Eq, $3); 
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_NEQ expr                                           
              { { 
                  def_expr with expr = E_Binop ($1, Neq, $3); 
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_AND expr                                           
              { { 
                  def_expr with expr = E_Binop ($1, And, $3); 
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_OR expr                                            
              { { 
                  def_expr with expr = E_Binop ($1, Or, $3); 
                  expr_pos = $1.expr_pos 
              } }
            | expr T_SMCOLON expr                                       
              { { 
                  def_expr with expr = E_Binop ($1, Semicolon, $3); 
                  expr_pos = $1.expr_pos 
              } } 
            | expr T_ASSIGN expr                                        
              { { 
                  def_expr with expr = E_Binop ($1, Assign, $3); 
                  expr_pos = $1.expr_pos 
              } }  
            | T_PLUS expr %prec UN                                      
              { { 
                  def_expr with expr = E_Unop (U_Plus, $2); 
                  expr_pos = $1.pos 
              } } 
            | T_FPLUS expr %prec UN                                     
              { { 
                  def_expr with expr = E_Unop (U_Fplus, $2); 
                  expr_pos = $1.pos 
              } } 
            | T_FMINUS expr %prec UN                                    
              { { 
                  def_expr with expr = E_Unop (U_Fminus, $2); 
                  expr_pos = $1.pos 
              } } 
            | T_MINUS expr %prec UN                                     
              { { 
                  def_expr with expr = E_Unop (U_Minus, $2); 
                  expr_pos = $1.pos 
              } } 
            | T_NOT expr %prec UN                                       
              { { 
                  def_expr with expr = E_Unop (U_Not, $2); 
                  expr_pos = $1.pos 
              } } 
            | T_DELETE expr %prec UN                                    
              { { 
                  def_expr with expr = E_Unop (U_Del, $2); 
                  expr_pos = $1.pos 
              } } 
            | T_BEGIN expr T_END                                        
              { { 
                  def_expr with expr = E_Block ($2); 
                  expr_pos = $1.pos 
              } }
            | T_WHILE expr T_DO expr T_DONE                             
              { { 
                  def_expr with expr = E_While ($2, $4); 
                  expr_pos = $1.pos 
              } }
            | T_FOR T_ID T_SEQ expr count expr T_DO expr T_DONE         
              { { 
                  def_expr with expr = E_For ($2.id_name, $4, $5, $6, $8); 
                  expr_pos = $1.pos 
              } }
            | T_DIM intmb T_ID                                          
              { { 
                  def_expr with expr = E_Dim ($2, $3.id_name); 
                  expr_pos = $1.pos 
              } }
            | T_NEW types                                               
              { { 
                  def_expr with expr = E_New ($2); 
                  expr_pos = $1.pos 
              } }
            | T_IF expr T_THEN expr T_ELSE expr                         
              { { 
                  def_expr with expr = E_Ifthenelse ($2, $4, $6); 
                  expr_pos = $1.pos 
              } }
            | T_IF expr T_THEN expr %prec IFX                           
              { { 
                  def_expr with expr = E_Ifthen ($2, $4); 
                  expr_pos = $1.pos 
              } }
            | letdef T_IN expr                                          
              { { 
                  def_expr with expr = E_Letin ($1, $3); 
                  expr_pos = $3.expr_pos 
              } }
            | T_MATCH expr T_WITH clause clausestar T_END               
              { { 
                  def_expr with expr = E_Match ($2, $4::(List.rev($5))); 
                  expr_pos = $1.pos 
              } } 
            | T_CID atomstar                                            
              { { 
                  def_expr with expr = E_Cid ($1.cid_name, $2); 
                  expr_pos = $1.cid_pos 
              } } 
            | T_ID atomstar                                             
              { { 
                  def_expr with expr = E_Id ($1.id_name, $2); 
                  expr_pos = $1.id_pos 
              } }
            | atom                                                      
              { { 
                  def_expr with expr = E_Atom ($1); 
                  expr_pos = $1.atom_pos 
              } } 
            ;

atom:       
            | T_INT                                                     
              { { 
                  def_atom with atom = A_Num $1.ival; 
                  atom_pos = $1.ipos 
              } }
            | T_FLOAT                                                   
              { { 
                  def_atom with atom = A_Dec $1.fval; 
                  atom_pos = $1.fpos 
              } }
            | T_CONSTCHAR                                               
              { { 
                  def_atom with atom = A_Chr $1.cval; 
                  atom_pos = $1.cpos 
              } }
            | T_STRING                                                  
              { { 
                  def_atom with atom = A_Str $1.sval; 
                  atom_pos = $1.spos 
              } } 
            | T_TRUE                                                    
              { { 
                  def_atom with atom = A_Bool $1.bval; 
                  atom_pos = $1.bpos 
              } } 
            | T_FALSE                                                   
              { { 
                  def_atom with atom = A_Bool $1.bval; 
                  atom_pos = $1.bpos 
              } } 
            | T_LPAR T_RPAR                                             
              { { 
                  def_atom with atom = A_Par; 
                  atom_pos = $1.pos 
              } } 
            | T_CID                                                     
              { { 
                  def_atom with atom = A_Cid $1.cid_name; 
                  atom_pos = $1.cid_pos 
              } } 
            | T_ID                                                      
              { { 
                  def_atom with atom = A_Var $1.id_name; 
                  atom_pos = $1.id_pos 
              } } 
            | T_BANK atom                                               
              { { 
                  def_atom with atom = A_Bang $2; 
                  atom_pos = $1.pos 
              } }
            | T_ID T_LBRACK expr comaexpr T_RBRACK                      
              { { 
                  def_atom with atom = A_Array ($1.id_name, $3::$4); 
                  atom_pos = $1.id_pos 
              } }
            | T_LPAR expr T_RPAR                                        
              { { 
                  def_atom with atom = A_Expr $2; 
                  atom_pos = $1.pos 
              } } 
            ;

comaexpr:  
            |  /* nothing */                                            
              { [] }  
            | T_COMA expr comaexpr                                      
              { $2::$3 }
            ;

            
atomstar:  
            | atom                                                      
              { [$1] } 
            | atom atomstar                                             
              { $1::$2 } 
            ;

count: 
            | T_TO                                                      
              { To }
            | T_DOWNTO                                                  
              { Downto }
            ;

intmb: 
            | /* nothing */                                             
              { None }
            | T_INT                                                     
              { Some ($1.ival) } 
            ;
clausestar:
            | /* nothing */                                              
              { [] }
            | clausestar T_BAR clause                                   
              { $3::$1 }
            ;

clause:
            | pattern T_GIVES expr                                      
              { Clause ($1, $3) }
            ;

pattern:
            | pattom                                                    
              { { 
                  pattern = Pa_Atom $1; 
                  pattern_pos = $1.pattom_pos; 
                  pattern_typ = T_Notype; 
                  pattern_entry = None 
              } }
            | T_CID pattomstar                                          
              { { 
                  pattern = Pa_Cid ($1.cid_name, List.rev($2)); 
                  pattern_pos = $1.cid_pos; 
                  pattern_typ = T_Notype; 
                  pattern_entry = None 
              } }
            ;

pattom:
            | T_PLUS T_INT %prec UN                                     
              { { 
                  pattom = P_Sign (P_Plus, $2.ival); 
                  pattom_pos = $1.pos; 
                  pattom_typ = T_Notype; 
                  pattom_entry = None 
              } } 
            | T_MINUS T_INT %prec UN                                    
              { { 
                  pattom = P_Sign (P_Minus, $2.ival); 
                  pattom_pos = $1.pos; 
                  pattom_typ = T_Notype; 
                  pattom_entry = None 
              } } 
            | T_FPLUS T_FLOAT %prec UN                                  
              { { 
                  pattom = P_Fsign (P_Fplus, $2.fval); 
                  pattom_pos = $1.pos; 
                  pattom_typ = T_Notype; 
                  pattom_entry = None 
              } } 
            | T_FMINUS T_FLOAT %prec UN                                 
              { { 
                  pattom = P_Fsign (P_Fminus, $2.fval); 
                  pattom_pos = $1.pos; 
                  pattom_typ = T_Notype; 
                  pattom_entry = None 
              } } 
            | T_INT                                                     
              { { 
                  pattom = P_Num $1.ival; 
                  pattom_pos = $1.ipos; 
                  pattom_typ = T_Notype; 
                  pattom_entry = None 
              } }
            | T_FLOAT                                                   
              { { 
                  pattom = P_Float $1.fval; 
                  pattom_pos = $1.fpos; 
                  pattom_typ = T_Notype; 
                  pattom_entry = None 
              } }
            | T_CONSTCHAR                                               
              { { 
                  pattom = P_Chr $1.cval; 
                  pattom_pos = $1.cpos; 
                  pattom_typ = T_Notype; 
                  pattom_entry = None 
              } }
            | T_TRUE                                                    
              { { 
                  pattom = P_Bool $1.bval; 
                  pattom_pos = $1.bpos; 
                  pattom_typ = T_Notype; 
                  pattom_entry = None 
              } }
            | T_FALSE                                                   
              { { 
                  pattom = P_Bool $1.bval; 
                  pattom_pos = $1.bpos; 
                  pattom_typ = T_Notype; 
                  pattom_entry = None 
              } }
            | T_ID                                                      
              { { 
                  pattom = P_Id $1.id_name; 
                  pattom_pos = $1.id_pos; 
                  pattom_typ = T_Notype; 
                  pattom_entry = None 
              } }
            | T_CID                                                     
              { { 
                  pattom = P_Cid $1.cid_name; 
                  pattom_pos = $1.cid_pos; 
                  pattom_typ = T_Notype; 
                  pattom_entry = None 
              } }
            | T_LPAR pattern T_RPAR                                     
              { { 
                  pattom = P_Pattern $2; 
                  pattom_pos = $1.pos; 
                  pattom_typ = T_Notype; 
                  pattom_entry = None 
              } }
            ;

pattomstar: 
            | pattom                                                    
              { [$1] }
            | pattomstar pattom                                         
              { $2::$1 }
            ;
%%
