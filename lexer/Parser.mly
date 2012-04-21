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

program     : stmt_list T_EOF                               { () }
            ;

stmt_list: 
            | /* nothing */                                  { () }
            | stmt_list letdef                               { print_endline "stmt_list letdef redused tou stmt_list"; () } 
            | stmt_list typedef                              { print_endline "stmt_list typedef redused tou stmt_list"; () } 
            ;

letdef:
            | T_LET recmb def anddef                        { print_endline "T_LET recmb def anddef reduced to  letdef"; () }
            ;

recmb:            
            | /* nothing */                                 { () }
            | T_REC                                         { print_endline "T_REC reduced to ercmb"; () }
            ;

anddef:
            | /* nothing */                                 { print_endline "nothing reduced to anddef "; () }
            | anddef T_ANDDEF def                           { print_endline "anddef T_AND def reduced to anddef"; () }
            ;

def:
            | T_ID parstar colontype T_SEQ expr              { print_endline "T_ID parstar colontype T_EQ exp reduced to def"; () }
            | T_MUTABLE T_ID colontype                       { print_endline "T_MUTABLE T_ID colontype reduced to def"; () }
            | T_MUTABLE T_ID T_LBRACK expr comaexpr T_RBRACK  colontype   
                                                            { print_endline "T_MUTABLE T_ID expr comaexpr colontype reduced to def"; () }
            ;

parstar:
            | /* nothing */                                 { print_endline "nothing reduced to parstar"; () }
            | parstar T_ID                                  { print_endline "T_ID reduced to parstar"; () }
            | parstar T_LPAR T_ID T_COLON types T_RPAR      { print_endline "T_LPAR T_ID T_COLON types T_RPAR reduced to parstar"; () }
            ;

colontype:
            | /* nothing */                                 { print_endline "nathing reduced to colontype"; () }
            | T_COLON types                                 { print_endline "reduced to "; () }
            ;

typedef:
            | T_TYPE tdef andtdefstar                       { print_endline "reduced to "; () }
            ;

andtdefstar:
            | /* nothing */                                 { print_endline "nathing reduced to andtdefstar"; () }
            | andtdefstar T_ANDDEF tdef                     { print_endline "tdefstar T_AND tdef reduced to andtdefstar "; () }

tdef: 
            | T_ID T_SEQ constr constrbar                   { print_endline "reduced to "; () }
            ;
            
constrbar:
            | /* nothing */                                 { print_endline "nathing reduced to constrbar"; () }
            | constrbar T_BAR constr                        { print_endline "constrbar T_BAR constr reduced to constrbar"; () }

constr:
            | T_CID oftype                                  { print_endline "T_ID oftype reduced to constr"; () }

oftype:
            | /* nothing */                                 { print_endline "nathing reduced to oftype"; () }
            | oftype T_OF typeplus                          { print_endline "oftype T_OF typeplus reduced to oftype "; () }
            ;

typeplus:
            | types                                         { print_endline "type reduced to typeplus"; () }
            | typeplus types                                { print_endline "typeplus types reduced to typeplus"; () }
            ;

types:
            | T_UNIT                                       { print_endline "T_UNIT reduced to type"; () }
            | T_INTST                                      { print_endline "T_INTST reduced to type"; () }
            | T_CHAR                                       { print_endline "T_CHAR reduced to type"; () }
            | T_BOOL                                       { print_endline "T_BOOL reduced to type"; () }
            | T_FLOATST                                    { print_endline "T_FLOATST reduced to type"; () }
            | T_LPAR types T_RPAR                          { print_endline "T_LPAR type T_RPAR reduced to type"; () }
            | types T_GIVES types                          { print_endline "types T_GIVES types reduced to type"; () }
            | types T_REF                                  { print_endline "types T_REF reduced to type"; () }
            | T_ARRAY starlist T_OF types %prec ARR        { print_endline "T_ARRAY starlist T_OF types reduced to type"; () }
            | T_ID                                         { print_endline "T_ID reduced to type"; () }
            ;
         
starlist:            
            | /* nothing */                                { print_endline "nathing reduced to starlist"; () }
            | T_LBRACK T_TIMES comastar T_RBRACK           { print_endline "T_LBRACK T_TIMES comastar T_RBRACK reduced to starlist"; () }
            ;

comastar:           
            | /* nothing */                                { print_endline "nathing reduced to starlist"; () }
            | comastar T_COMA T_TIMES                      { print_endline "commastar T_COMA T_TIMES reduced to comastar"; () }
            
expr:
            | expr T_FPLUS expr                            { print_endline "expr T_FPLUS expr reduced to expr"; () } 
            | expr T_PLUS expr                             { print_endline "expr T_PLUS expr reduced to expr"; () } 
            | expr T_MINUS expr                            { print_endline "expr T_MINUS expr reduced to expr"; () } 
            | expr T_FMINUS expr                           { print_endline "expr T_FMINUS expr reduced to expr"; () } 
            | expr T_TIMES expr                            { print_endline "expr T_TIMES expr reduced to expr"; () } 
            | expr T_FTIMES expr                           { print_endline "expr T_FTIMES expr reduced to expr"; () } 
            | expr T_DIV expr                              { print_endline "expr T_DIV expr reduced to expr"; () } 
            | expr T_FDIV expr                             { print_endline "expr T_FDIV expr reduced to expr"; () } 
            | expr T_MOD expr                              { print_endline "expr T_MOD expr reduced to expr"; () } 
            | expr T_POWER expr                            { print_endline "expr T_POWER expr reduced to expr"; () } 
            | expr T_SEQ expr                              { print_endline "expr T_SEQ expr reduced to expr"; () } 
            | expr T_NSEQ expr                             { print_endline "expr T_NSEQ expr reduced to expr"; () } 
            | expr T_L expr                                { print_endline "expr T_L expr reduced to expr"; () } 
            | expr T_G expr                                { print_endline "expr T_G expr reduced to expr"; () } 
            | expr T_LE expr                               { print_endline "expr T_LE expr reduced to expr"; () } 
            | expr T_GE expr                               { print_endline "expr T_GE expr reduced to expr"; () } 
            | expr T_EQ expr                               { print_endline "expr T_EQ expr reduced to expr"; () } 
            | expr T_NEQ expr                              { print_endline "expr T_NEQ expr reduced to expr"; () } 
            | expr T_AND expr                              { print_endline "expr T_AND expr reduced to expr"; () } 
            | expr T_OR expr                               { print_endline "expr T_OR expr reduced rop expr"; () } 
            | expr T_SMCOLON expr                          { print_endline "expr T_SMCOLON expr reduced to expr"; () } 
            | expr T_ASSIGN expr                           { print_endline "expr T_ASSIGN expr reduced to expr"; () }  
            | T_PLUS expr %prec UN                         { print_endline "T_PLUS unexpr reduced to unexp"; () } 
            | T_FPLUS expr %prec UN                        { print_endline "T_FPLUS unexpr reduced to unexp"; () } 
            | T_FMINUS expr %prec UN                       { print_endline "T_FMINUS unexprreduced to unexp" ;() } 
            | T_MINUS expr %prec UN                        { print_endline "T_MINUS unexpr reduced to unexp";() } 
            | T_DELETE expr                                { print_endline "delete expr reduced to expr"; () } 
            | T_BEGIN expr T_END                           { print_endline "begin expr reduced end reduced to expr"; () }
            | T_WHILE expr T_DO expr T_DONE                { print_endline "WHILE expr DO expr DONE redused to expr"; () }
            | T_FOR T_ID T_SEQ expr count expr T_DO expr T_DONE 
                                                           { print_endline "T_FOR T_ID T_SEQ expr count expr T_DO expr reduced to expr"; () }
            | T_DIM intmb T_ID                             { print_endline "dim intmb id reduced to expr"; () }
            | T_NEW types                                  { print_endline "T_NEW types reduced to expr"; () }
            | T_IF expr T_THEN expr T_ELSE expr            { print_endline "T_IF expr T_THEN expr T_ELSE expr reduced to expr "; () }
            | T_IF expr T_THEN expr                        { print_endline "T_IF expr T_THEN expr reduced to expr "; () }
            | letdef T_IN expr                             { print_endline "letdef T_IN expr reduced to expr "; () }
            | T_MATCH expr T_WITH clause clausestar T_END  { print_endline "T_MATCH expr T_WITH clause clausestar T_END reduced to expr"; () } 
            | T_CID atomstar                               { print_endline "cid bigexpr reduced to bigexpr"; () } 
            | T_ID atomstar                                { print_endline "id bigexpr reduced to bigexpr"; () } 
            | atom                                         { print_endline "atom reduced to expr"; () }            
            ;

atom:       
            | T_INT                                        { print_endline "int reduced to atom";() } 
            | T_FLOAT                                      { print_endline "float reduced to app"; () } 
            | T_CONSTCHAR                                  { print_endline "float reduced to app"; () } 
            | T_STRING                                     { print_endline "string reduced to app"; () } 
            | T_TRUE                                       { print_endline "true reduced to atom"; () } 
            | T_FALSE                                      { print_endline "false reduced to app"; () } 
            | T_LPAR T_RPAR                                { print_endline "unit reduced to app"; () } 
            | T_CID                                        { print_endline "cid reduced to atom"; () } 
            | T_ID                                         { print_endline "id reduced to atom"; () } 
            | T_NOT atom                                   { print_endline "T_NOT atom reduced to atom"; () }
            | T_BANK atom                                  { print_endline "T_BANK atom reduced to atom"; () }
            | T_ID T_LBRACK expr comaexpr T_RBRACK         { print_endline "id lbrack expr comaexpr rbrac reduced to atom"; () } 
            | T_LPAR expr T_RPAR                           { print_endline "lpar expr rpar reduced to atom"; () } 
           ;

comaexpr:  
            |  /* nothing */                               { print_endline "nothing reduced to comaexpr"; () }  
            | comaexpr T_COMA expr                         { print_endline "comaexpr tcoma bigexpr reduced to comaexpr"; () } 
            ;

            
atomstar:  
            | atom                                          { () } 
            | atom atomstar                                 { () } 
            ;


count: 
            | T_TO                                          { print_endline "T_TO reduced to count "; () }
            | T_DOWNTO                                      { print_endline "T_DOWNTO reduced to count"; () }
            ;

intmb: 
            | /* nothing */                                 { () }
            | T_INT                                         { () } 
            ;

clausestar:
            | /*nathing */                                  { () }
            | clausestar T_BAR clause                       { print_endline "clausestar T_BAR clause reduced to clausestar"; () }
            ;

clause:
            | pattern T_GIVES expr                          { print_endline "Patern T_GIVES EXPR reduced to clause"; () }
            ;

pattern:
            | pattom                                        { print_endline "Patom Redused to pattern"; ()}
            | T_CID patternstar                             { print_endline "T_ID patternstar reduced to pattern"; () }


pattom:
            | sign T_INT                                    { print_endline "sign T_INT reduced to pattern"; () }
            | fsign T_FLOAT                                 { print_endline "sign T_FLOAT reduced to pattern"; () }
            | T_CONSTCHAR                                   { print_endline "T_CONSTCHAR reduced to pattern"; () }
            | T_TRUE                                        { print_endline "T_TRUE reduced to pattern"; () }
            | T_FALSE                                       { print_endline "T_FALSE reduced to pattern"; () }
            | T_ID                                          { print_endline "T_ID reduced to pattern"; () }
            | T_CID                                          { print_endline "T_CID reduced to pattern"; () }
            | T_LPAR pattern T_RPAR                         { print_endline "T_LPAR pattern T_RPAR reduced to pattern"; () }
            ;

patternstar:
            | pattom                                        { print_endline "Pattom reduced to patternstar"; () }
            | patternstar pattom                            { print_endline"patternstar pattom reduced to patternstar"; () }
            ;

sign:
            | T_PLUS %prec UN                               { print_endline "T_PLUS reduced to sign"; () } 
            | T_MINUS %prec UN                              { print_endline "T_MINUS reduced to sign"; () } 
            ;


fsign:
            | T_FPLUS %prec UN                               { print_endline "T_FPLUS reduced to sign"; () } 
            | T_FMINUS %prec UN                              { print_endline "T_FMINUS reduced to sign"; () } 
            ;

%% 
