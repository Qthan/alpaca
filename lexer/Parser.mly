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

%token T_ID
%token T_CID

%token T_FLOAT
%token T_INT

%token T_CHAR
%token T_CONSTCHAR
%token T_STRING
%token T_FALSE
%token T_TRUE
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

%%

program     : stmt_list T_EOF                                           { () }
            ;

stmt_list: 
            | /* nothing */                                             { () }
            | stmt_list letdef                                          { () } 
            | stmt_list typedef                                         { () } 
            ;

letdef:
            | T_LET recmb def anddef                                    { () }
            ;

recmb:            
            | /* nothing */                                             { () }
            | T_REC                                                     { () }
            ;

anddef:
            | /* nothing */                                             { () }
            | anddef T_ANDDEF def                                       { () }
            ;

def:
            | T_ID parstar colontype T_SEQ expr                         { () }
            | T_MUTABLE T_ID colontype                                  { () }
            | T_MUTABLE T_ID T_LBRACK expr comaexpr T_RBRACK  colontype   
                                                                        { () }
            ;

parstar:
            | /* nothing */                                             { () }
            | parstar T_ID                                              { () }
            | parstar T_LPAR T_ID T_COLON types T_RPAR                  { () }
            ;

colontype:
            | /* nothing */                                             { () }
            | T_COLON types                                             { () }
            ;

typedef:
            | T_TYPE tdef andtdefstar                                   { () }
            ;

andtdefstar:
            | /* nothing */                                             { () }
            | andtdefstar T_ANDDEF tdef                                 { () }

tdef: 
            | T_ID T_SEQ constr constrbar                               { () }
            ;
            
constrbar:
            | /* nothing */                                             { () }
            | constrbar T_BAR constr                                    { () }

constr:
            | T_CID oftype                                              { () }

oftype:
            | /* nothing */                                             { () }
            | oftype T_OF typeplus                                      { () }
            ;

typeplus:
            | types                                                     { () }
            | typeplus types                                            { () }
            ;

types:
            | T_UNIT                                                    { () }
            | T_INTST                                                   { () }
            | T_CHAR                                                    { () }
            | T_BOOL                                                    { () }
            | T_FLOATST                                                 { () }
            | T_LPAR types T_RPAR                                       { () }
            | types T_GIVES types                                       { () }
            | types T_REF                                               { () }
            | T_ARRAY starlist T_OF types %prec ARR                     { () }
            | T_ID                                                      { () }
            ;
         
starlist:            
            | /* nothing */                                             { () }
            | T_LBRACK T_TIMES comastar T_RBRACK                        { () }
            ;

comastar:           
            | /* nothing */                                             { () }
            | comastar T_COMA T_TIMES                                   { () }
            
expr:
            | expr T_FPLUS expr                                         { () } 
            | expr T_PLUS expr                                          { () } 
            | expr T_MINUS expr                                         { () } 
            | expr T_FMINUS expr                                        { () } 
            | expr T_TIMES expr                                         { () } 
            | expr T_FTIMES expr                                        { () } 
            | expr T_DIV expr                                           { () } 
            | expr T_FDIV expr                                          { () } 
            | expr T_MOD expr                                           { () } 
            | expr T_POWER expr                                         { () } 
            | expr T_SEQ expr                                           { () } 
            | expr T_NSEQ expr                                          { () } 
            | expr T_L expr                                             { () } 
            | expr T_G expr                                             { () } 
            | expr T_LE expr                                            { () } 
            | expr T_GE expr                                            { () } 
            | expr T_EQ expr                                            { () } 
            | expr T_NEQ expr                                           { () } 
            | expr T_AND expr                                           { () } 
            | expr T_OR expr                                            { () } 
            | expr T_SMCOLON expr                                       { () } 
            | expr T_ASSIGN expr                                        { () }  
            | T_PLUS expr %prec UN                                      { () } 
            | T_FPLUS expr %prec UN                                     { () } 
            | T_FMINUS expr %prec UN                                    { () } 
            | T_MINUS expr %prec UN                                     { () } 
            | T_DELETE expr                                             { () } 
            | T_BEGIN expr T_END                                        { () }
            | T_WHILE expr T_DO expr T_DONE                             { () }
            | T_FOR T_ID T_SEQ expr count expr T_DO expr T_DONE 
                                                                        { () }
            | T_DIM intmb T_ID                                          { () }
            | T_NEW types                                               { () }
            | T_IF expr T_THEN expr T_ELSE expr                         { () }
            | T_IF expr T_THEN expr                                     { () }
            | letdef T_IN expr                                          { () }
            | T_MATCH expr T_WITH clause clausestar T_END               { () } 
            | T_CID atomstar                                            { () } 
            | T_ID atomstar                                             { () } 
            | atom                                                      { () }            
            ;

atom:       
            | T_INT                                                     { () } 
            | T_FLOAT                                                   { () } 
            | T_CONSTCHAR                                               { () } 
            | T_STRING                                                  { () } 
            | T_TRUE                                                    { () } 
            | T_FALSE                                                   { () } 
            | T_LPAR T_RPAR                                             { () } 
            | T_CID                                                     { () } 
            | T_ID                                                      { () } 
            | T_NOT atom                                                { () }
            | T_BANK atom                                               { () }
            | T_ID T_LBRACK expr comaexpr T_RBRACK                      { () } 
            | T_LPAR expr T_RPAR                                        { () } 
           ;

comaexpr:  
            |  /* nothing */                                            { () }  
            | comaexpr T_COMA expr                                      { () } 
            ;

            
atomstar:  
            | atom                                                      { () } 
            | atom atomstar                                             { () } 
            ;


count: 
            | T_TO                                                      { () }
            | T_DOWNTO                                                  { () }
            ;

intmb: 
            | /* nothing */                                             { () }
            | T_INT                                                     { () } 
            ;

clausestar:
            | /*nathing */                                              { () }
            | clausestar T_BAR clause                                   { () }
            ;

clause:
            | pattern T_GIVES expr                                      { () }
            ;

pattern:
            | pattom                                                    { () }
            | T_CID patternstar                                         { () }


pattom:
            | sign T_INT                                                { () }
            | fsign T_FLOAT                                             { () }
            | T_CONSTCHAR                                               { () }
            | T_TRUE                                                    { () }
            | T_FALSE                                                   { () }
            | T_ID                                                      { () }
            | T_CID                                                     { () }
            | T_LPAR pattern T_RPAR                                     { () }
            ;

patternstar:
            | pattom                                                    { () }
            | patternstar pattom                                        { () }
            ;

sign:
            | T_PLUS %prec UN                                           { () } 
            | T_MINUS %prec UN                                          { () } 
            ;


fsign:
            | T_FPLUS %prec UN                                          { () } 
            | T_FMINUS %prec UN                                         { () } 
            ;

%% 
