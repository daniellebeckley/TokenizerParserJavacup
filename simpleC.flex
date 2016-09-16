/* Danielle Beckley
 * CSE 5343
 * SimpleC Scanner
 */
import java_cup.runtime.*;

%%

%public
%class Scanner
%implements sym

%line
%column

%cup
%cupdebug

%{
  StringBuffer string = new StringBuffer();
  
  private Symbol symbol(int type) {
    return new MySymbol(type, yyline+1, yycolumn+1);
  }

  private Symbol symbol(int type, Object value) {
    return new MySymbol(type, yyline+1, yycolumn+1, value);
  }
%}

/* White spaces */
WhiteSpace = [ \t\f\r\n]

/* Comments in C files: no need to worry about them*/

/* Identifiers: general, as described in Section 6.4.2.1 in the ANSI C document. 
 * Ignore "universal-character-name" and "other implementation-defined characters"
 * 
 */


Identifier = [_A-Za-z][_A-Za-z0-9]*

/* Integer literals: integer literals described in
 * Section 6.4.4.1 of the ANSI C document. For simplicity, do NOT
 * handle the 'long long' cases. Handles long, unsigned,
 * hexadecimal, and octal constants.
 */

DecInteger = [1-9][0-9]*
DecIntegerLiteral = {DecInteger} [uU]
DecIntegerLiteralLong = {DecInteger} ([lL]?[uU]? | [uU]?[lL]?)

OctalInteger =  0[0-7]*
OctalIntegerLiteral = {OctalInteger} [uU]
OctalLiteralLong = {OctalInteger} ([lL]?[uU]? | [uU]?[lL]?)

HexadecimalInteger = 0[xX][A-Fa-f1-9]+
HexadecimalIntegerLiteral = {HexadecimalInteger} [uU]
HexadecimalLiteralLong = {HexadecimalInteger} ([lL]?[uU]? | [uU]?[lL]?)



/* Floating point literals: floating point literals 
 * described in Section 6.4.4.2 of the ANSI C document. For
 * simplicity, do NOT handle the 'long double' cases. Handles
 * hexadecimal floating constants, e/E and p/P notation, and f/F
 * suffixes.
 */        


exponent_part = [eE][+-]?[0-9]+

DoubleLiteral  = [0-9]+ \. [0-9]* {exponent_part}?  | [0-9]+{exponent_part} | \. [0-9]+ {exponent_part}?
DoubleLiteralLong = {DoubleLiteral}[lL]
LiteralFloat = {DoubleLiteral}[fF]


HexDigit = [A-Fa-f0-9]
pNotation = [pP][+-]?[0-9]+
HexadecimalFP = 0[xX]{HexDigit}* \.? {HexDigit}+ {pNotation} | 0[xX] {HexDigit}+ {pNotation}
HexadecimalFPLong = {HexadecimalFP} [lL]
HexadecimalFPFloat = {HexadecimalFP} [fF]

%%

<YYINITIAL> {

  /* Keywords: all keywords from Section 6.4.1 */

 "auto"                         { return symbol(AUTO); }
  "break"                        { return symbol(BREAK); }
  "case"                         { return symbol(CASE); }
  "char"                         { return symbol(CHAR); }
  "const"                        { return symbol(CONST); }
  "continue"                     { return symbol(CONTINUE); }
  "default"                      { return symbol(DEFAULT); }
  "do"                           { return symbol(DO); }
  "double"                       { return symbol(DOUBLE); }
  "else"                         { return symbol(ELSE); }
  "enum"                         { return symbol(ENUM); }
  "extern"                       { return symbol(EXTERN); }
  "float"                        { return symbol(FLOAT); }
  "for"                          { return symbol(FOR); }
  "goto"                         { return symbol(GOTO); } 
  "if"                           { return symbol(IF); }
  "inline"                       { return symbol(INLINE); }
  "int"                          { return symbol(INT); }
  "long"                         { return symbol(LONG); }
  "register"                     { return symbol(REGISTER); }
  "restrict"                     { return symbol(RESTRICT); }
  "return"                       { return symbol(RETURN); }
  "short"                        { return symbol(SHORT); }
  "signed"                       { return symbol(SIGNED); }
  "sizeof"                       { return symbol(SIZEOF); }
  "static"                       { return symbol(STATIC); }
  "struct"                       { return symbol(STRUCT); }
  "switch"                       { return symbol(SWITCH); }
  "typedef"                      { return symbol(TYPEDEF); }
  "union"                        { return symbol(UNION); }
  "unsigned"                     { return symbol(UNSIGNED); }
  "void"                         { return symbol(VOID); }
  "volatile"                     { return symbol(VOLATILE); }
  "while"                        { return symbol(WHILE); }
  "_Bool"                        { return symbol(_BOOL); }
  "_Complex"                     { return symbol(_COMPLEX); }
  "_Imaginary"                   { return symbol(_IMAGINARY); }
  
  /* Punctuators: all punctuators from Section 6.4.6 except last 8 */


  "("                            { return symbol(LPAREN); }
  ")"                            { return symbol(RPAREN); }
  "{"                            { return symbol(LBRACE); }
  "}"                            { return symbol(RBRACE); }
  "["                            { return symbol(LBRACK); }
  "]"                            { return symbol(RBRACK); }
  "."                            { return symbol(DOT); }
  "->"                           { return symbol(ARROW); }
  "++"                           { return symbol(PLUSPLUS); }
  "--"                           { return symbol(MINUSMINUS); }
  "&"                            { return symbol(AND); }
  "*"                            { return symbol(STAR); }
  "+"                            { return symbol(PLUS); }
  "-"                            { return symbol(MINUS); }
  "~"                            { return symbol(TILDA); }
  "!"                            { return symbol(EXCLAMATION); }
  "/"                            { return symbol(DIV); }
  "%"                            { return symbol(PERCENT); }
  "<<"                           { return symbol(LESSLESS); }
  ">>"                           { return symbol(GREATERGREATER); }
  "<"                            { return symbol(LESS); }
  ">"                            { return symbol(GREATER); }
  "<="                           { return symbol(LESSEQUAL); }
  ">="                           { return symbol(GREATEREQUAL); }
  "=="                           { return symbol(EQUALEQUAL); }
  "!="                           { return symbol(NOTEQUAL); }
  "^"                            { return symbol(CARET); }
  "|"                            { return symbol(OR); }
  "&&"                           { return symbol(ANDAND); }
  "||"                           { return symbol(OROR); }
  "?"                            { return symbol(QUESTION); }
  ":"                            { return symbol(COLON); }
  ";"                            { return symbol(SEMICOLON); }
  "..."                          { return symbol(DOTDOTDOT); }
  "="                            { return symbol(ASSGN); }
  "*="                           { return symbol(STARASSGN); }
  "/="                           { return symbol(DIVASSGN); }
  "%="                           { return symbol(PERCENTASSGN); }
  "+="                           { return symbol(ADDASSGN); }
  "-="                           { return symbol(MINUSASSGN); }
  "<<="                          { return symbol(LESSASSGN); }
  ">>="                          { return symbol(GREATERASSGN); }
  "&="                           { return symbol(ANDASSGN); }
  "^="                           { return symbol(CARETASSGN); }
  "|="                           { return symbol(ORASSGN); }
  ","                            { return symbol(COMMA); }

  
  /* Integer literals: any such literal, the token type is
   * INTEGER_LITERAL. The attribute value should be either a
   * Java Integer object or a Java Long object. 
   *
   * For example, if
   * the literal is "17", "0x11", or "021", you should create a Java
   * Integer object containing the Java 'int' value 17. 
  */
  
  {DecInteger}                	     { return symbol(INTEGER_LITERAL, new Integer(yytext())); }
  {DecIntegerLiteral}                { return symbol(INTEGER_LITERAL, Integer.valueOf(yytext().replaceAll("[uU]",""),10)); }
  {DecIntegerLiteralLong}            { return symbol(INTEGER_LITERAL, Long.valueOf(yytext().replaceAll("[uUlL]","") ,10)); }

  {OctalInteger}            	     { return symbol(INTEGER_LITERAL, Integer.valueOf(yytext(), 8)); }
  {OctalIntegerLiteral}              { return symbol(INTEGER_LITERAL, Integer.valueOf(yytext().replaceAll("[uU]",""), 8)); }
  {OctalLiteralLong}                 { return symbol(INTEGER_LITERAL, Long.valueOf(yytext().replaceAll("[uUlL]",""), 8)); }

  {HexadecimalInteger}               { return symbol(INTEGER_LITERAL, Integer.valueOf(yytext().substring(2),16)); }
  {HexadecimalIntegerLiteral}        { return symbol(INTEGER_LITERAL, Integer.valueOf(yytext().replaceAll("[uU]","").substring(2), 16)); }
  {HexadecimalLiteralLong}    	     { return symbol(INTEGER_LITERAL, Long.valueOf(yytext().replaceAll("[uUlL]","").substring(2), 16)); }


  /* Floating-point literals: any such literal, the token is FLOATING_POINT_LITERAL.
   * The attribute value should be either a Java Float object or a Java Double object
   * 
   * For example, if the literal is ".5625f", "5625e-4f", or "0X1.2p-1f",
   * you should create a Java Float object containing the Java 'float'
   * value 0.5625.
   */

  
  {DoubleLiteral}                	{ return symbol(FLOATING_POINT_LITERAL, Double.valueOf(yytext())); }
  {DoubleLiteralLong}            	{ return symbol(FLOATING_POINT_LITERAL, Float.valueOf(yytext().replaceAll("[lL]",""))); }
  {LiteralFloat}           		{ return symbol(FLOATING_POINT_LITERAL, Float.valueOf(yytext().replaceAll("[fF]",""))); }

  {HexadecimalFP}                       { return symbol(FLOATING_POINT_LITERAL, Double.valueOf(yytext())); }
  {HexadecimalFPLong}             	{ return symbol(FLOATING_POINT_LITERAL, Double.valueOf(yytext().replaceAll("[lL]",""))); }
  {HexadecimalFPFloat}            	{ return symbol(FLOATING_POINT_LITERAL, Float.valueOf(yytext().replaceAll("[fF]",""))); } 

  /* whitespace */
  {WhiteSpace}                   { /* ignore */ }

  /* identifiers */ 
  {Identifier}                   { return symbol(IDENTIFIER, yytext()); }  
}

/* error fallback */
.|\n                             { throw new RuntimeException("Illegal character \""+yytext()+
                                                              "\" at line "+(yyline+1)+", column "+(yycolumn+1)); }
<<EOF>>                          { return symbol(EOF); }
