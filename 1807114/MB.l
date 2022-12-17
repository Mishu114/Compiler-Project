%{
	#include<stdio.h>
	#include<string.h>
	#include<stdlib.h>
	#include<math.h>
	#include<stdarg.h>
	#include "MB.tab.h"
%}

alpha [a-zA-Z][a-zA-Z0-9]*
digit [0-9]
keyWord					RETURN|INTGR|FLOATING|CHARACS
singleLineComment		[ ]*[#][#][a-zA-Z0-9@$%^&*(){}_+-,.:\|?>< ]*


%%


"main"	   { return MAIN; }
"integer_num"  			 {return INT;}
"borno" 		         {return CHAR;}
"float_num" 			 	{return FLOAT;}
"poriborton" 		   { return SWITCH; }
"dekhano" 		       { return PF; }
"shorto"   		   { return CASE;  } 
"jodi" 			   { return IF;   }
"abar_jodi"			{return ELIF;}
"nahole" 		   { return ELSE; }
"vanga"  		   { return BREAK;  }
"dflt" 	       { return DEFAULT; }
"{" 		   { return START;  }
"}"  		   { return END;	}
"aborton" 		   { return LOOP;}
"jog"			{return PLUS;}
"biyog"			{return MINUS;}
"gun"			{return MUL;}
"vag"			{return DIV;}
"vagshes"		{return MOD;}
"boro"			{return GT;}
"choto"			{return LT;}
"shuchok"		{return POW;}

SIN			   {return SIN;}
COS            {return COS;}
TAN            {return TAN;}
LN			   {return LOG;}
LOG 		   {return LOG10;}

{singleLineComment}		{return COM;}


{digit}+	{ yylval.number = atoi(yytext);	 return NUM; }
{alpha}	    { yylval.string = strdup(yytext);	 return VAR; }
[-+/*<>=,():;%^]     { return *yytext;	}
[ ]*	    		 	{}
[\n]*					{}
[\t]*					{}
.						{printf("%s --> invalid\n",yytext);}


%%
