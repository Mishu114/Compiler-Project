%{
	#include<stdio.h>
	#include<stdlib.h>
	#include<math.h>
	#include<string.h>
	#include<stdarg.h>
	int data[60];
	int yylex();
	extern FILE *yyin,*yyout;
	int i,f=0;
	typedef struct variable {
			char *str;
	    		int n;
			}array;
	array store[1000];
	void yyerror(char *s);
	void vari (array *p, char *s, int n);
	void val_assign(char *s, int n);
	int check(char *key);
	int count = 1,cnt = 1,sw=0;
	int q=0,prev=0;
	float fl;
%}

%union 
{
	 int number;
     char *string;
}

%token <string> VAR
%type <number> expression statement 
%token <number> NUM
%token MAIN PLUS MINUS MUL DIV MOD INT FLOAT CHAR COM START END SWITCH CASE DEFAULT BREAK IF ELIF ELSE LOOP PF SIN COS TAN LOG LOG10 LT GT POW
%nonassoc IF
%nonassoc ELSE
%nonassoc SWITCH
%nonassoc CASE
%nonassoc DEFAULT
%right '='
%left LT GT
%left PLUS MINUS
%left MUL DIV MOD


%%

program: MAIN START cstatement END
	 ;

cstatement: /* NULL */
	| cstatement statement
	;
statement: ';'		{}	
	| declaration ';'		{ 
		printf("\nDeclaration\n"); 
		}

	| expression ';' 			{}
	
	| VAR '=' expression ';' { 
								if(check($1))
								{
									val_assign($1,$3);
									printf("\nVariable %s = %d\t\n",$1,$3);
								}
								else
								{
									printf("\n(%s) Variable Not Declared\n",$1); 
								}
							} 
	| COM	{
				printf("This is a single line comment.\n");
		}
   
	| LOOP '(' expression LT expression ';' expression '+''+' ')' START expression '=' expression ';' END {
	                                int i;
	                                for(i=$3 ; i<$5 ; i++) {printf("iteration %d: value= %d\n", i,$14);}									
				               }
	| LOOP '(' expression GT expression ';' expression '-''-' ')' START expression '=' expression ';' END {
	                                int i;
	                                for(i=$3 ; i>$5 ; i--) {printf("iteration %d: value= %d\n", i,$14);}									
				               }
	| LOOP '(' expression LT expression ';' expression '-''-' ')' START expression '=' expression ';' END {
	                                int i;
	                                for(i=$5 ; i>$3 ; i--) {printf("iteration %d: value= %d\n", i,$14);}									
				               }
	| LOOP '(' expression GT expression ';' expression '+''+' ')' START expression '=' expression ';' END {
	                                int i;
	                                for(i=$5 ; i<$3 ; i++) {printf("iteration %d: value= %d\n", i,$14);}									
				               }
	| SWITCH '(' expression ')' START B  END {}

	| IF '(' expression ')' START expression ';' END %prec IFX {
								if($3){
									printf("\nvalue: %d\n",$6);
								}
								else{
									printf("condition is not true\n");
								}
							}

	| IF '(' expression ')' START expression ';' END ELIF '(' expression ')' START expression ';' END ELSE START expression ';' END {
								if($3){
									printf("value(IF): %d\n",$6);
								}
								else if($11){
									printf("value(ELIF): %d\n",$14);
								}
								else{
									printf("value(ELSE): %d\n",$19);
								}
							}
	| PF '(' expression ')' ';' {printf("%d",$3);}
	;
	
B   : C
	| C D
    ;
C   : C '+' C
	| CASE NUM ':' expression ';' BREAK ';' { 
		printf("CASE %d -> %d\n",$2, $4); 
		}
	;
D   : DEFAULT ':' expression ';' BREAK ';' { 
	printf("CASE (Default) -> %d\n",$3); 
	}
;

expression: NUM					{ $$ = $1; 	}

	| VAR 	{	int i = 1;
				char *name = store[i].str;
				while (name) 
				{
					if (strcmp(name, $1) == 0)
					{
						$$ = (int)store[i].n;
						//printf("%s -> %d\n", $1, (int)store[i].n ) ;
						break;
					}
						name = store[++i].str;
				}
			}						
	
	| expression PLUS expression	{ $$ = $1 + $3; }

	| expression MINUS expression	{ $$ = $1 - $3; }

	| expression MUL expression	{ $$ = $1 * $3; }

	| expression DIV expression	{ if($3){
				     					$$ = $1 / $3;
				  					}
				  					else{
										$$ = 0;
										printf("\ndivision by zero\t");
				  					} 	
				    			}
	| expression MOD expression	{ if($3){
				     					$$ = $1 % $3;
				  					}
				  					else{
										$$ = 0;
										printf("\nMOD by zero\t");
				  					} 	
				    			}
	| expression POW expression	{ $$ = pow($1 , $3);}
	| expression LT expression	{ $$ = $1 < $3; }
	
	| expression GT expression	{ $$ = $1 > $3; }

	| '(' expression ')'		{ $$ = $2;	}
	| SIN expression 			{
		printf("Value of Sin(%d) is %lf\n",$2,sin($2*3.1416/180)); 
		$$=sin($2*3.1416/180);
		}

    | COS expression 			{
		printf("Value of Cos(%d) is %lf\n",$2,cos($2*3.1416/180)); 
		$$=cos($2*3.1416/180);
		}

    | TAN expression 			{
		printf("Value of Tan(%d) is %lf\n",$2,tan($2*3.1416/180)); 
		$$=tan($2*3.1416/180);
		}

    | LOG10 expression 			
	{printf("Value of log(%d) is %lf\n",$2,(log($2*1.0)/log(10.0))); 
	$$=(log($2*1.0)/log(10.0));
	}
	| LOG expression 			{
		printf("Value of ln(%d) is %lf\n",$2,(log($2))); 
		$$=(log($2));
		}
	
;
	
declaration : TYPE ID1 
             ;


TYPE : INT   
     | FLOAT  
     | CHAR   
     ;



ID1 : ID1 ',' VAR '=' NUM	 {
				if(check($3))
				{
					printf("\n(%s) Variable  DEclared Before \n",$3);
				}
				else
				{ 
					val_assign($3,$5);
					printf("\nVariable %s = %d\n",$3,$5);
				}
		}
	| ID1 ',' VAR 	{				if(check($3))
						{
							printf("\nERROR:Multiple Declaration Of (%s) \n", $3 );
						}
						else
						{
							printf("Variable (%s) Declared\n",$3);
							vari(&store[count],$3, count);
							count++;
						}
			}
	| VAR '=' NUM	 {
				if(check($1))
				{
					printf("\n(%s) Variable  DEclared Before \n",$1);
				}
				else
				{ 
					val_assign($1,$3);
					printf("\nVariable %s = %d\n",$1,$3);
				}
		}
	| VAR 	{				if(check($1))
						{
							printf("\nERROR:Multiple Declaration Of (%s) \n", $1 );
						}
						else
						{
							printf("Variable (%s) Declared\n",$1);
							vari(&store[count],$1, count);
							count++;
						}
			} 
;
  



%%

void vari(array *p, char *s, int n)
				{
				  p->str = s;
				  p->n = n;
				}
void val_assign(char *s, int num)
			{
				    int i = 1;
				    char *name = store[i].str;
				    while (name) {
				        if (strcmp(name, s) == 0){
					store[i].n=num;
						break;
				            }
					name = store[++i].str;
				}
			}

int check(char *key)
			{
				
			    int i = 1;
			    char *name = store[i].str;
			    while (name) {
				        if (strcmp(name, key) == 0){
						return i;
					}
						name = store[++i].str;
				}
			    return 0;
			}
void yyerror(char *s){
	printf( "%s\n", s);
}
int yywrap()
{
	return 1;
}

int main()
{
	freopen("input.txt","r",stdin);
	freopen("output.txt","w",stdout);
	yyparse();

	fclose(yyin);
 	fclose(yyout);
    
	return 0;
}

