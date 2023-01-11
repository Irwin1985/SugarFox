* ================================================================================== *
* Scanner class
* ================================================================================== *
#ifndef CONSTANTS_LOADED
	#include "SugarFox.h"
#endif
Define Class Scanner As Custom
	Hidden cSource
	Hidden nCursor
	Hidden nTokenCounter
	Hidden nLastToken
	Hidden oRegEx
	Hidden oTokens
	Hidden nLine
	Hidden nCol
	Dimension aSpecs[100]

	Function Init(tcSource)
		If Right(tcSource, 1) != Chr(10)
			tcSource = tcSource + CRLF
		EndIf
		With This
			.cSource = tcSource
			.nCursor = 1
			.nTokenCounter = 0
			.nLastToken = 0
			.oTokens = Createobject('Collection')
			* Setting the oRegEx properties
			.oRegEx = Createobject("VBScript.RegExp")
			.oRegEx.IgnoreCase = .F.
			.oRegEx.Global = .T.
			.nLine = 1
			.nCol = 1

			Local i, lcPattern
			i = 1
			* -----------------------------------------------------------
			* Whitespace:
			.aSpecs[i] = Createobject("Spec", "^[ \t\r\f]+", TT_IGNORE, TC_IGNORABLE)

			* -----------------------------------------------------------
			* NewLine
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\n+", TT_SEMICOLON, TC_GENERIC)

			* -----------------------------------------------------------
			* Comments:
			* Skip single-line comments
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^#.*", TT_IGNORE, TC_IGNORABLE)

			* Skip multi-line comments
			i = i + 1
			.aSpecs[i] = Createobject("Spec", '^"""[\s\S]*?"""', TT_IGNORE, TC_IGNORABLE)

			* -----------------------------------------------------------
			* Numbers:
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\d+[_.\d]*", TT_NUMBER, TC_LITERAL)

			* -----------------------------------------------------------
			* Double quoted string:
			i = i + 1
			.aSpecs[i] = Createobject("Spec", '^"[^"]*"', TT_STRING, TC_LITERAL)
			
			* -----------------------------------------------------------
			* Double quoted string with scape character:
			i = i + 1
			lcPattern = '^\"(?:[^\"\\^' + Chr(39) + '\\]|\\.)*\"'
			.aSpecs[i] = Createobject("Spec", lcPattern, TT_STRING, TC_LITERAL)			

			* -----------------------------------------------------------
			* Single quoted string:
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^'[^']*'", TT_STRING, TC_LITERAL)

			* -----------------------------------------------------------
			* Single quoted string with scape characters:
			i = i + 1
			lcPattern = "^'(?:[^\" + Chr(34) + "\\^'\\]|\\.)*'"
			.aSpecs[i] = Createobject("Spec", lcPattern, TT_STRING, TC_LITERAL)

			* -----------------------------------------------------------
			* Backticked string:
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^`[^`]*`", TT_STRING, TC_LITERAL)

			* -----------------------------------------------------------
			* Relational Operators:
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^[<>]=?", TT_RELATIONAL_OPERATOR, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^[=!]=", TT_EQUALITY_OPERATOR, TC_GENERIC)

			* -----------------------------------------------------------
			* Logical Operators:
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\band\b", TT_LOGICAL_AND, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bor\b", TT_LOGICAL_OR, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^!", TT_LOGICAL_NOT, TC_UNARY)

			* -----------------------------------------------------------
			* Keywords:
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bconst\b", TT_CONST, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bdo\b", TT_DO, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bas\b", TT_AS, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bfalse\b", TT_FALSE, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bpublic\b", TT_PUBLIC, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bprivate\b", TT_PRIVATE, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bbreak\b", TT_BREAK, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bcontinue\b", TT_CONTINUE, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\belse\b", TT_ELSE, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bguard\b", TT_GUARD, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bimport\b", TT_IMPORT, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bnew\b", TT_NEW, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bprint\b", TT_PRINT, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bthis\b", TT_THIS, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bthisform\b", TT_THISFORM, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\blocal\b", TT_LOCAL, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bcase\b", TT_CASE, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\benum\b", TT_ENUM, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bfor\b", TT_FOR, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bin\b", TT_IN, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bnull\b", TT_NULL, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\breturn\b", TT_RETURN, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bthrow\b", TT_THROW, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bcatch\b", TT_CATCH, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bfunc\b", TT_FUNCTION, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bend\b", TT_END, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\btry\b", TT_TRY, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bwhile\b", TT_WHILE, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bclass\b", TT_CLASS, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bif\b", TT_IF, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\bswitch\b", TT_SWITCH, TC_KEYWORD)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\btrue\b", TT_TRUE, TC_KEYWORD)

			* -----------------------------------------------------------
			* Assignment operators: =, +=, -=, *=, /=
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^=", TT_SIMPLE_ASSIGN, TC_ASSIGNMENT)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^[\+\-\*\/]=", TT_COMPLEX_ASSIGN, TC_ASSIGNMENT)

			* -----------------------------------------------------------
			* Math operators: +, -, *, /
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^[\+\-]", TT_TERM_OPERATOR, TC_UNARY)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^[\*\/]", TT_FACTOR_OPERATOR, TC_GENERIC)

			* -----------------------------------------------------------
			* Identifier
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\w+", TT_IDENTIFIER, TC_IDENTIFIER)

			* -----------------------------------------------------------
			* Symbol and Delimiters:
			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\(", TT_LPAREN, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\)", TT_RPAREN, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\[", TT_LBRACKET, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\]", TT_RBRACKET, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\{", TT_LBRACE, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\}", TT_RBRACE, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^\.", TT_DOT, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^,", TT_COMMA, TC_GENERIC)

			i = i + 1
			.aSpecs[i] = Createobject("Spec", "^:", TT_COLON, TC_GENERIC)

			* Shrink the array
			Dimension .aSpecs[i]
		endwith
	Endfunc

	Function scanTokens
		Local loToken
		* Debug
		Local lnTokenAnt, i
		lnTokenAnt = 0
		i = 0
		* Debug
		Do While .T.
			loToken = This.getNextToken()
			If Isnull(loToken)
				Exit
			EndIf
			* Debug
			If loToken.Type != lnTokenAnt
				i = 0
				lnTokenAnt = loToken.Type
			EndIf
			If lnTokenAnt = loToken.Type
				i = i + 1
			EndIf
			If i >= 20
				Set Step On
			EndIf 
			* Debug
			This.oTokens.Add(loToken)
		Enddo
		This.oTokens.Add(Createobject("Token", TT_EOF, TC_GENERIC, "", "", This.nLine, This.nCol))

		Return This.oTokens
	Endfunc

	Hidden Function getNextToken
		If This.nCursor > Len(This.cSource)
			Return .Null.
		Endif
		Local lcInput, i, loMatcher, lcLexeme, ln, loToken
		lcInput = Substr(This.cSource, This.nCursor)
		For Each loSpec In This.aSpecs
			* loSpec = this.aSpecs[i]
			This.oRegEx.Pattern = loSpec.cPattern

			loMatcher = This.oRegEx.Execute(lcInput)
			If Type('loMatcher') != 'O' Or Empty(loMatcher.Count)
				Loop
			Endif
			* Increase cursor to the length of matched string.
			This.nCursor = This.nCursor + loMatcher.Item[0].Length
			lcLexeme = loMatcher.Item[0].Value

			* Count number of lines
			ln = Len(lcLexeme) - Len(Strtran(lcLexeme, Chr(10)))
			This.nLine = This.nLine + ln
			If ln > 0
				This.nCol = 1
			Endif

			* check for the IGNORE token type.
			If loSpec.nType == TT_IGNORE
				This.nCol = This.nCol + Len(lcLexeme) && update column number
				Return This.getNextToken()
			Endif
			* Check for new line
			If loSpec.nType == TT_SEMICOLON
				If this.nLastToken == TT_SEMICOLON or this.nLastToken == TT_COMMA or Empty(this.nTokenCounter)
					Return this.getNextToken()
				EndIf
				this.nLastToken = TT_SEMICOLON
				lcLexeme = ";"
			Else
				this.nLastToken = loSpec.nType
			EndIf
			this.nTokenCounter = this.nTokenCounter + 1

			* Return the token and value
			Local lvValue, lnCategory
			lnCategory = loSpec.nCategory
			Do Case
			Case loSpec.nType == TT_NUMBER
				lcLexeme = Strtran(lcLexeme, '_')
				lvValue = Val(lcLexeme)
			Case loSpec.nType == TT_TRUE
				lvValue = .T.
			Case loSpec.nType == TT_FALSE
				lvValue = .F.
			Case loSpec.nType == TT_NULL
				lvValue = .Null.
			Case loSpec.nType == TT_STRING
				If Left(lcLexeme,1) == '`' && raw string
					lcLexeme = Substr(lcLexeme, 2, Len(lcLexeme)-2)
				Else
					*lcLexeme = Substr(lcLexeme, 2, Len(lcLexeme)-2)
					lcLexeme = Strtran(lcLexeme, '\r', Chr(13))
					lcLexeme = Strtran(lcLexeme, '\n', Chr(10))
					lcLexeme = Strtran(lcLexeme, '\t', Chr(9))
					lcLexeme = Strtran(lcLexeme, '\"', '"')
					lcLexeme = Strtran(lcLexeme, "\'", "'")
					lcLexeme = Strtran(lcLexeme, '\\', "\")
				Endif
				lvValue = lcLexeme
			Case Inlist(loSpec.nType, TT_COMPLEX_ASSIGN, TT_TERM_OPERATOR, TT_FACTOR_OPERATOR, TT_RELATIONAL_OPERATOR, TT_EQUALITY_OPERATOR, TT_LOGICAL_NOT)
				Do Case
				Case Inlist(lcLexeme, "+", "+=")
					lnCategory = TC_PLUS
				Case Inlist(lcLexeme, "-", "-=")
					lnCategory = TC_MINUS
				Case Inlist(lcLexeme, "*", "*=")
					lnCategory = TC_MUL
				Case Inlist(lcLexeme, "/", "/=")
					lnCategory = TC_DIV
				Case lcLexeme == "="
					lnCategory = TC_ASSIGN
				Case lcLexeme == "<"
					lnCategory = TC_LESS
				Case lcLexeme == "<="
					lnCategory = TC_LESS_EQ
				Case lcLexeme == ">"
					lnCategory = TC_GREATER
				Case lcLexeme == ">="
					lnCategory = TC_GREATER_EQ
				Case lcLexeme == "=="
					lnCategory = TC_EQUAL
				Case lcLexeme == "!"
					lnCategory = TC_BANG
				Case lcLexeme == "!="
					lnCategory = TC_NOT_EQ
				Endcase
			Otherwise
				lvValue = lcLexeme
			Endcase
			loToken = Createobject("Token", loSpec.nType, lnCategory, lcLexeme, lvValue, This.nLine, This.nCol)
			This.nCol = This.nCol + Len(lcLexeme)

			Return loToken
		Endfor
		_Screen.FoxScript.errorLine(This.nLine, This.nCol, "Unknown character: " + Substr(lcInput, 1, 1))
		Return .Null.
	Endfunc

Enddefine

* ================================================================================== *
* The Specification class for regular exp.
* ================================================================================== *
Define Class Spec As Custom
	nType = 0
	cPattern = ''
	nCategory = 0
	Function Init(tcPattern, tnType, tnCategory)
		This.nType = tnType
		This.cPattern = tcPattern
		This.nCategory = tnCategory
	Endfunc
Enddefine

* ================================================================================== *
* The Token class
* ================================================================================== *
Define Class Token As Custom
	Type = 0
	category = 0
	lexeme = ''
	literal = .Null.
	Line = 0
	Col = 0

	Function Init(tnType, tnCategory, tcLexeme, tnLiteral, tnLine, tnCol)
		This.Type = tnType
		This.category = tnCategory
		This.lexeme = tcLexeme
		This.literal = tnLiteral
		This.Line = tnLine
		This.Col = tnCol
	Endfunc

	Function toString
		Return foxScriptTokenToStr(This.Type) + "[" + Alltrim(Str(This.Line)) + ":" + Alltrim(Str(This.Col)) + "]<lexeme: '" + This.lexeme + "'>"
	Endfunc
Enddefine

* ================================================================================== *
* helper functions
* ================================================================================== *
Function foxScriptTokenToStr(tnTokenType)
	Do Case
	Case tnTokenType == TT_EOF
		Return "TT_EOF"
	Case tnTokenType == TT_IGNORE
		Return "TT_IGNORE"
	Case tnTokenType == TT_SEMICOLON
		Return "TT_SEMICOLON"
	Case tnTokenType == TT_RELATIONAL_OPERATOR
		Return "TT_RELATIONAL_OPERATOR"
	Case tnTokenType == TT_EQUALITY_OPERATOR
		Return "TT_EQUALITY_OPERATOR"
	Case tnTokenType == TT_EQUALITY_OPERATOR
		Return "TT_EQUALITY_OPERATOR"
	Case tnTokenType == TT_SIMPLE_ASSIGN
		Return "TT_SIMPLE_ASSIGN"
	Case tnTokenType == TT_COMPLEX_ASSIGN
		Return "TT_COMPLEX_ASSIGN"
	Case tnTokenType == TT_NUMBER
		Return "TT_NUMBER"
	Case tnTokenType == TT_STRING
		Return "TT_STRING"
	Case tnTokenType == TT_IDENTIFIER
		Return "TT_IDENTIFIER"
	Case tnTokenType == TT_LPAREN
		Return "TT_LPAREN"
	Case tnTokenType == TT_RPAREN
		Return "TT_RPAREN"
	Case tnTokenType == TT_LBRACKET
		Return "TT_LBRACKET"
	Case tnTokenType == TT_RBRACKET
		Return "TT_RBRACKET"
	Case tnTokenType == TT_LBRACE
		Return "TT_LBRACE"
	Case tnTokenType == TT_RBRACE
		Return "TT_RBRACE"
	Case tnTokenType == TT_DOT
		Return "TT_DOT"
	Case tnTokenType == TT_COMMA
		Return "TT_COMMA"
	Case tnTokenType == TT_COLON
		Return "TT_COLON"
	Case tnTokenType == TT_LOGICAL_AND
		Return "TT_LOGICAL_AND"
	Case tnTokenType == TT_LOGICAL_OR
		Return "TT_LOGICAL_OR"
	Case tnTokenType == TT_LOGICAL_NOT
		Return "TT_LOGICAL_NOT"
	Case tnTokenType == TT_TERM_OPERATOR
		Return "TT_TERM_OPERATOR"
	Case tnTokenType == TT_FACTOR_OPERATOR
		Return "TT_FACTOR_OPERATOR"
	Case tnTokenType == TT_CONST
		Return "TT_CONST"
	Case tnTokenType == TT_DO
		Return "TT_DO"
	Case tnTokenType == TT_FALSE
		Return "TT_FALSE"
	Case tnTokenType == TT_AS
		Return "TT_AS"
	Case tnTokenType == TT_PUBLIC
		Return "TT_PUBLIC"
	Case tnTokenType == TT_PRIVATE
		Return "TT_PRIVATE"
	Case tnTokenType == TT_BREAK
		Return "TT_BREAK"
	Case tnTokenType == TT_CONTINUE
		Return "TT_CONTINUE"
	Case tnTokenType == TT_ELSE
		Return "TT_ELSE"
	Case tnTokenType == TT_GUARD
		Return "TT_GUARD"
	Case tnTokenType == TT_IMPORT
		Return "TT_IMPORT"
	Case tnTokenType == TT_NEW
		Return "TT_NEW"
	Case tnTokenType == TT_PUBLIC
		Return "TT_PUBLIC"
	Case tnTokenType == TT_THIS
		Return "TT_THIS"
	Case tnTokenType == TT_THISFORM
		Return "TT_THISFORM"
	Case tnTokenType == TT_LOCAL
		Return "TT_LOCAL"
	Case tnTokenType == TT_CASE
		Return "TT_CASE"
	Case tnTokenType == TT_ENUM
		Return "TT_ENUM"
	Case tnTokenType == TT_FOR
		Return "TT_FOR"
	Case tnTokenType == TT_IN
		Return "TT_IN"
	Case tnTokenType == TT_NULL
		Return "TT_NULL"
	Case tnTokenType == TT_RETURN
		Return "TT_RETURN"
	Case tnTokenType == TT_THROW
		Return "TT_THROW"
	Case tnTokenType == TT_CATCH
		Return "TT_CATCH"
	Case tnTokenType == TT_FUNCTION
		Return "TT_FUNCTION"
	Case tnTokenType == TT_END
		Return "TT_END"
	Case tnTokenType == TT_TRY
		Return "TT_TRY"
	Case tnTokenType == TT_WHILE
		Return "TT_WHILE"
	Case tnTokenType == TT_CLASS
		Return "TT_CLASS"
	Case tnTokenType == TT_IF
		Return "TT_IF"
	Case tnTokenType == TT_PRINT
		Return "TT_PRINT"
	Case tnTokenType == TT_SWITCH
		Return "TT_SWITCH"
	Case tnTokenType == TT_TRUE
		Return "TT_TRUE"
	Otherwise
		Return "UNKNOWN"
	Endcase
Endfunc