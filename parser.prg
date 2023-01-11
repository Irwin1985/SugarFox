* ================================================================================== *
* The Parser Class
* ================================================================================== *

#ifndef CONSTANTS_LOADED
	#include "SugarFox.h"
#endif

Define Class Parser As Custom
	Hidden oTokens
	Hidden nCurrent
	Hidden previous
	Hidden peek
	Hidden isAtEnd

	Function Init(toTokens)
		This.oTokens = toTokens
		This.nCurrent = 1 && First recognised token
	Endfunc

	Function parse
		Local loStatements
		loStatements = Createobject('Collection')

		Do While !This.isAtEnd
			loStatements.Add(This.declaration())
		Enddo

		Return loStatements
	Endfunc

	Hidden Function declaration
		Local loDeclaration, lSeguir
		loDeclaration = .Null.
		lSeguir = .T.
		Try
			If This.match(TT_CLASS)
				lSeguir = .F.
				loDeclaration = This.classDeclaration()
			EndIf
			If lSeguir and This.check(TT_FUNCTION)
				Local lnType1
				lnType1 = This.peekNext(1)
				If lnType1 == TT_IDENTIFIER && function declaration
					this.match(TT_FUNCTION)
					lSeguir = .F.
					loDeclaration = This.functionDeclaration("function")
				EndIf
			EndIf
			If lSeguir and this.match(TT_ENUM)
				lSeguir = .F.
				loDeclaration = this.enumDeclaration()
			EndIf
			if lSeguir and (This.match(TT_LOCAL) or This.match(TT_PRIVATE) or This.match(TT_PUBLIC))
				lSeguir = .F.
				loDeclaration = This.variableDeclaration()
			EndIf
			if lSeguir and This.match(TT_GUARD)
				lSeguir = .F.
				loDeclaration = This.guardDeclaration()
			EndIf			
			If lSeguir
				loDeclaration = This.statement()
			EndIf
		Catch To loEx
			If Type('loEx.Type') == 'C'
				This.synchronize()
			Else
				* DEBUG
				Local lcMsg
				lcMsg = "ERROR NRO: " + Alltrim(Str(loEx.ErrorNo))
				lcMsg = lcMsg + Chr(13) + "LINEA: "  	+ Alltrim(Str(loEx.Lineno))
				lcMsg = lcMsg + Chr(13) + "MESSAGE: "  	+ Alltrim(loEx.Message)
				lcMsg = lcMsg + Chr(13) + "LUGAR: "  	+ Alltrim(loEx.Procedure)
				Messagebox(lcMsg, 16)
				* DEBUG
			Endif
		Endtry
		Return loDeclaration
	Endfunc

	Hidden Function classDeclaration
		Local loName, loSuperClass, loMethods
		loName = This.consume(TT_IDENTIFIER, "Expect class name.")
		loSuperClass = .Null.

		If This.match(TT_AS) && this class extends from another class (inheritance)
			This.consume(TT_IDENTIFIER, "Expect super class name.")
			loSuperClass = Createobject("Variable", This.previous)
		Endif

		This.consume(TT_SEMICOLON, "Expect newline before class body.")

		loMethods = Createobject('Collection')
		Do While This.match(TT_FUNCTION)
			loMethods.Add(This.functionDeclaration("method"))
		Enddo

		This.consume(TT_END, "Expect 'end' after class body.")
		This.consume(TT_SEMICOLON, "Expect newline after class declaration.")

		Return Createobject("ClassNode", loName, loSuperClass, loMethods)
	Endfunc

	Hidden Function statement
		If This.match(TT_FOR)
			Return This.forStatement()
		Endif
		If This.match(TT_IF)
			Return This.ifStatement()
		Endif
		If This.match(TT_PRINT)
			Return This.printStatement()
		Endif
		If This.match(TT_RETURN)
			Return This.returnStatement()
		Endif
		If This.match(TT_WHILE)
			Return This.whileStatement()
		EndIf
		If this.match(TT_DO)
			Return this.doWhileStatement()
		EndIf
		If this.match(TT_IMPORT)
			Return this.importStatement()
		EndIf
*!*			If this.check(TT_IDENTIFIER)
*!*				Local lnTypeNext
*!*				lnTypeNext = this.peekNext(1)
*!*				If lnTypeNext == TT_SIMPLE_ASSIGN or lnTypeNext == TT_COMPLEX_ASSIGN
*!*					Return this.assignment()
*!*				EndIf
*!*			endif

		Return This.expressionStatement()
	Endfunc
	
	Hidden Function forStatement
		Local loKeyword, loIndexOrKey, loValue, loStop, loBody
		lokeyword = this.previous
		Store .null. to loIndexOrKey, loValue, loStop, loBody
		
		If This.match(TT_LPAREN)
			loIndexOrKey = this.expression()
			This.consume(TT_RPAREN, "Expect ')' after expression.")
		Else
			loIndexOrKey = this.expression()
		endif
		If this.match(TT_COMMA)
			loValue = this.expression()
		EndIf
		this.consume(TT_IN, "Expect 'in' keyword.")
		loStop = this.expression()		
		
		loBody = this.blockStatement()
		
		Return CreateObject("ForNode", loKeyword, loIndexOrKey, loValue, loStop, CreateObject("Block", loBody))
	Endfunc

	Hidden Function ifStatement
		Local loCondition, loThenBranch, loElseBranch
		loThenBranch = Createobject('Collection')
		loElseBranch = .Null.
		
		If this.match(TT_LPAREN)
			loCondition = This.Expression()
			This.consume(TT_RPAREN, "Expect ')' after if condition.")
		Else
			loCondition = This.Expression()
		EndIf
		
		* Parse until 'end' or 'else' tokens				
		This.consume(TT_SEMICOLON, "Expect newline before block.")
		
		Do While (!This.Check(TT_END) and !This.Check(TT_ELSE)) And !This.isAtEnd
			loThenBranch.Add(This.declaration())
		Enddo
		* Parse

		If This.match(TT_ELSE)			
			loElseBranch = This.blockStatement()
		Else
			This.consume(TT_END, "Expect 'end' after block.")
			This.consume(TT_SEMICOLON, "Expect newline after expression.")
		Endif

		Return Createobject("IfNode", loCondition, CreateObject("Block", loThenBranch), CreateObject("Block", loElseBranch))
	Endfunc

	Hidden Function printStatement
		Local loExpression
		loExpression = This.Expression()
		This.consume(TT_SEMICOLON, "Expect newline after value.")
		Return Createobject("Print", loExpression)
	Endfunc

	Hidden Function returnStatement
		Local loKeyword, loValue
		loKeyword = This.previous
		loValue = .Null.
		If !This.Check(TT_SEMICOLON)
			loValue = This.Expression()
		Endif
		This.consume(TT_SEMICOLON, "Expect newline after return value.")
		Return Createobject("ReturnNode", loKeyword, loValue)
	Endfunc

	Hidden Function variableDeclaration
		Local loKeyword, loName, loInitializer
		loKeyword = this.previous
		loName = This.consume(TT_IDENTIFIER, "Expect variable name.")
		loInitializer = .Null.
		If This.match(TT_SIMPLE_ASSIGN)
			loInitializer = This.Expression()
		Endif
		This.consume(TT_SEMICOLON, "Expect newline after variable declaration.")

		Return Createobject("VariableDecl", loKeyword, loName, loInitializer)
	Endfunc

	Hidden Function whileStatement
		
		Local loCondition, loBody
		If this.match(TT_LPAREN)
			loCondition = This.Expression()			
			This.consume(TT_RPAREN, "Expect ')' after condition.")	
		Else
			loCondition = This.Expression()
		EndIf
		loBody = This.Blockstatement()
		Return Createobject("WhileNode", loCondition, CreateObject("Block", loBody))
	Endfunc

	Hidden Function doWhileStatement

		Local loCondition, loBody
		
		* Parse until 'while'
		loBody = CreateObject("Collection")
		This.consume(TT_SEMICOLON, "Expect newline before do block.")
		
		Do While !This.Check(TT_WHILE) And !This.isAtEnd
			loBody.Add(This.declaration())
		Enddo
		* end parse
		This.consume(TT_WHILE, "Expect 'while' keyword.")
		
		If this.match(TT_LPAREN)
			loCondition = This.Expression()			
			This.consume(TT_RPAREN, "Expect ')' after condition.")	
		Else
			loCondition = This.Expression()
		EndIf
		This.consume(TT_SEMICOLON, "Expect newline after do block.")
		Return Createobject("DoWhile", loCondition, CreateObject("Block", loBody))
	Endfunc

	Hidden Function importStatement		
		Local loKeyword, loFileName
		loKeyword = this.previous
		loFileName = This.consume(TT_IDENTIFIER, "Expect file name to import.")
		Return Createobject("Import", lokeyword, loFileName)
	Endfunc

	Hidden Function expressionStatement	
		Local loExpr
		loExpr = This.Expression()
		If this.match(TT_SIMPLE_ASSIGN, TT_COMPLEX_ASSIGN)
			Local loOperator, loValue
			loOperator = this.previous
			loValue = this.expression()
			
			Do Case
			Case loExpr.Class == 'Variable'
				loExpr = Createobject("Assign", loOperator, loExpr.oName, loValue)
			Case loExpr.Class == 'Get'
				loExpr = Createobject("Set", loExpr.oObject, loExpr.oName, loValue)
			Otherwise
				This.parseError(loEquals, "Invalid assignment target.")
			EndCase
		EndIf
		This.consume(TT_SEMICOLON, "Expect newline after expression.")
		Return Createobject("Expression", loExpr)
	Endfunc

	Hidden Function functionDeclaration(tcKind)
		Local loName, loParams, loBody, llMatchRightParen
		loName = This.consume(TT_IDENTIFIER, "Expect " + tcKind + " name.")
		llMatchRightParen = this.match(TT_LPAREN)
		loParams = Createobject('Collection')
		If !This.Check(TT_RPAREN)
			loParams.Add(This.consume(TT_IDENTIFIER, "Expect parameter name."))
			Do While This.match(TT_COMMA)
				loParams.Add(This.consume(TT_IDENTIFIER, "Expect parameter name."))
			Enddo
		EndIf
		If llMatchRightParen
			This.consume(TT_RPAREN, "Expect ')' after parameters.")
		EndIf
		* Parse Body
		loBody = This.blockStatement()

		Return Createobject("FunctionNode", loName, loParams, CreateObject("Block", loBody))
	EndFunc
	
	Hidden function enumDeclaration
		Local loKeyword, loName, loElements, i, loNumber
		loKeyword = this.previous
		loName = this.consume(TT_IDENTIFIER, "Expect enum name.")
		this.consume(TT_SEMICOLON, "Expect newline before enum elements.")

		loElements = CreateObject("Dictionary")
		i = 1
		loIdentifier = this.consume(TT_IDENTIFIER, "Expect element name")
		If this.match(TT_SIMPLE_ASSIGN)
			loNumber = this.consume(TT_NUMBER, "Expect numeric literal value for this element.")
			i = loNumber.literal
		EndIf
		loElements.put(loIdentifier.lexeme, i)
		
		Do while this.match(TT_COMMA)
			i = i + 1
			loIdentifier = this.consume(TT_IDENTIFIER, "Expect element name")
			If this.match(TT_SIMPLE_ASSIGN)
				loNumber = this.consume(TT_NUMBER, "Expect numeric literal value for this element.")
				i = loNumber.literal
			EndIf
			loElements.put(loIdentifier.lexeme, i)
		EndDo
		This.consume(TT_SEMICOLON, "Expect newline after last element of the enum.")
		This.consume(TT_END, "Expect 'end' keyword after enum element definition.")
		This.consume(TT_SEMICOLON, "Expect newline after 'end' keyword.")
		
		Return CreateObject("Enum", loKeyword, loName, loElements)
	EndFunc
	
	Hidden FUNCTION guardDeclaration
		Local loKeyword, loCondition, loBody
		loKeyword = this.previous
		
		If this.match(TT_LPAREN)
			loCondition = this.Expression()
			this.consume(TT_RPAREN, "Expect ')' after condition.")
		Else
			loCondition = this.Expression()
		EndIf
		
		this.consume(TT_ELSE, "Expect 'else' keyword.")
		
		loBody = this.blockStatement()
		
		Return CreateObject("Guard", loKeyword, loCondition, CreateObject("Block", loBody))
	EndFunc

	Hidden Function blockStatement(tlLetNewLineActive)
		Local loStatements
		loStatements = Createobject('Collection')
		
		This.consume(TT_SEMICOLON, "Expect newline before block.")
		
		Do While !This.Check(TT_END) And !This.isAtEnd
			loStatements.Add(This.declaration())
		Enddo
		
		This.consume(TT_END, "Expect 'end' keyword after block.")
		If !tlLetNewLineActive
			This.consume(TT_SEMICOLON, "Expect newline after block.")
		EndIf
		
		Return loStatements
	Endfunc

	* ==========================================================
	* Parsing Expression
	Hidden Function Expression
		Return This.logicalOr()
	Endfunc

	Hidden Function assignment
		Local loExpr, loNode, loOperator
		loExpr = This.logicalOr()

		If this.check(TT_SIMPLE_ASSIGN)
			loOperator = This.consume(TT_SIMPLE_ASSIGN, "Expect `=` after variable name.")
		Else
			loOperator = This.consume(TT_COMPLEX_ASSIGN, "Expect assignment operator after variable name.")
		EndIf
		Local loEquals, loValue
		loEquals = This.previous && catch the equal token
		loValue = This.expression()
		
		Do Case
		Case loExpr.Class == 'Variable'
			loNode = Createobject("Assign", loOperator, loExpr.oName, loValue)
		Case loExpr.Class == 'Get'
			loNode = Createobject("Set", loExpr.oObject, loExpr.oName, loValue)
		Otherwise
			This.parseError(loEquals, "Invalid assignment target.")
		EndCase
		This.consume(TT_SEMICOLON, "Expect newline after variable declaration.")

		Return loNode
	Endfunc

	Hidden Function logicalOr
		Local loLeft
		loLeft = This.logicalAnd()
		Do While This.match(TT_LOGICAL_OR)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.logicalAnd()
			loLeft  = Createobject("Logical", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function logicalAnd
		Local loLeft
		loLeft = This.equality()
		Do While This.match(TT_LOGICAL_AND)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.equality()
			loLeft  = Createobject("Logical", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function equality
		Local loLeft
		loLeft = This.comparison()
		Do While This.match(TT_EQUALITY_OPERATOR)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.comparison()
			loLeft  = Createobject("Binary", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function comparison
		Local loLeft
		loLeft = This.Term()
		Do While This.match(TT_RELATIONAL_OPERATOR)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.Term()
			loLeft  = Createobject("Binary", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function term
		Local loLeft
		loLeft = This.factor()
		Do While This.match(TT_TERM_OPERATOR)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.factor()
			loLeft  = Createobject("Binary", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function factor
		Local loLeft
		loLeft = This.unary()
		Do While This.match(TT_FACTOR_OPERATOR)
			Local loOpe, loRight
			loOpe   = This.previous
			loRight = This.unary()
			loLeft  = Createobject("Binary", loLeft, loOpe, loRight)
		Enddo
		Return loLeft
	Endfunc

	Hidden Function unary
		If This.match(TT_TERM_OPERATOR, TT_LOGICAL_NOT)
			Return Createobject("Unary", This.previous, This.unary())
		Endif
		Return This.Call()
	Endfunc

	Hidden Function finishCall(toCallee)
		Local loArguments, loParen
		loArguments = Createobject('Collection')
		If !This.Check(TT_RPAREN)
			loArguments.Add(This.Expression())
			Do While This.match(TT_COMMA)
				loArguments.Add(This.Expression())
			Enddo
		Endif
		loParen = This.consume(TT_RPAREN, "Expect ')' after arguments.")
		Return Createobject("Call", toCallee, loParen, loArguments)
	Endfunc

	Hidden Function Call
		Local loExpr, loName, loKeyword, loIndex
		loExpr = This.Primary()

		Do While .T.
			Do Case
			Case This.match(TT_LPAREN)
				loExpr = This.finishCall(loExpr)
			Case This.match(TT_DOT)
				loName = This.consume(TT_IDENTIFIER, "Expect property name after '.'.")
				loExpr = Createobject("Get", loExpr, loName)
			Case This.match(TT_LBRACKET)
				loKeyword = this.previous
				loIndex = This.Expression()
				This.consume(TT_RBRACKET, "Expect ']' after expression.")
				loExpr = CreateObject("ArrayAccess", loKeyword, loExpr, loIndex)			
			Otherwise
				Exit
			Endcase
		Enddo
		Return loExpr
	Endfunc

	Hidden Function Primary		
		Do Case
		Case This.match(TT_FALSE)
			Return Createobject("Literal", .F.)
		Case This.match(TT_TRUE)
			Return Createobject("Literal", .T.)
		Case This.match(TT_NULL)
			Return Createobject("Literal", .Null.)
		Case This.match(TT_NUMBER, TT_STRING)
			Return Createobject("Literal", This.previous.literal)
		Case This.match(TT_THIS)
			Return Createobject("ThisNode", This.previous)
		Case This.match(TT_THISFORM)
			Return Createobject("ThisformNode", This.previous)
		Case This.match(TT_IDENTIFIER)
			Return Createobject("Variable", This.previous)
		Case This.match(TT_LPAREN)
			Local loExpressions
			loExpressions = CreateObject('Collection')
			If !this.check(TT_RPAREN)
				loExpressions.Add(This.Expression())
				Do while this.match(TT_COMMA)
					loExpressions.Add(This.Expression())
				EndDo
			EndIf
			This.consume(TT_RPAREN, "Expect ')' after expression.")
			Return Createobject("Grouping", loExpressions)
		CASE This.match(TT_NEW)
			Return this.parseNewExpression()			
		Case This.match(TT_LBRACKET)
			Return This.parseArrayLiteral()
		Case This.match(TT_LBRACE)
			Return This.parseObject()
		Case This.match(TT_FUNCTION)		
			Return This.parseFunctionExpr()
		Otherwise
			This.parseError(This.peek, "Expect expression.")
		Endcase
	Endfunc

	Hidden Function parseArrayLiteral
		Local loKeyword, loElements
		loKeyword = This.previous && token '['

		this.match(TT_SEMICOLON) && optionally match a new line
		
		loElements = Createobject('Dictionary')
		If !This.Check(TT_RBRACKET)
			loElements.Add(This.Expression())
			Do While This.match(TT_COMMA)
				loElements.Add(This.Expression())
			Enddo
		EndIf
		this.match(TT_SEMICOLON) && optionally match a new line
		This.consume(TT_RBRACKET, "Expect ']' after array elements.")

		Return Createobject("ArrayLiteral", loKeyword, loElements)
	Endfunc

	Hidden Function parseObject
		Local loKeyword, loElements, loProperty, loValue
		loKeyword = This.previous && token '{'

		this.match(TT_SEMICOLON) && optionally match a new line
		
		loElements = Createobject('Dictionary')
		If !This.Check(TT_RBRACE)
			loProperty = This.Expression()
			If !This.checkProperty(loProperty)
				This.parseError(loKeyword, "Invalid property name.")
			Endif
			This.consume(TT_COLON, "Expect ':' after property name.")
			loValue = This.Expression()
			loElements.Add(loValue, This.getPropertyName(loProperty))

			Do While This.match(TT_COMMA)
				loProperty = This.Expression()
				If !This.checkProperty(loProperty)
					This.parseError(loKeyword, "Invalid property name.")
				Endif
				This.consume(TT_COLON, "Expect ':' after property name.")
				loValue = This.Expression()
				loElements.Add(loValue, This.getPropertyName(loProperty))
			Enddo
		EndIf
		
		this.match(TT_SEMICOLON) && optionally match a new line
		This.consume(TT_RBRACE, "Expect '}' after array elements.")

		Return Createobject("ObjectLiteral", loKeyword, loElements)
	Endfunc

	Hidden Function parseFunctionExpr
		Local loParams, loBody
		This.consume(TT_LPAREN, "Expect '(' after function name.")

		loParams = Createobject('Collection')
		If !This.Check(TT_RPAREN)
			loParams.Add(This.consume(TT_IDENTIFIER, "Expect parameter name."))
			Do While This.match(TT_COMMA)
				loParams.Add(This.consume(TT_IDENTIFIER, "Expect parameter name."))
			Enddo
		Endif
		This.consume(TT_RPAREN, "Expect ')' after parameters.")

		* Parse Body
		loBody = This.blockStatement(.T.)

		Return Createobject("FunctionExpr", loParams, loBody)
	EndFunc
	
	Hidden function parseNewExpression
		Local loKeyword, loClassName, loArguments
		loKeyword = this.previous
		loClassName = this.consume(TT_IDENTIFIER, "Expect class name.")
		this.consume(TT_LPAREN, "Expect '(' after class name.")
		loArguments = CreateObject('Collection')
		If !This.Check(TT_RPAREN)
			loArguments.Add(this.expression())
			Do While This.match(TT_COMMA)
				loArguments.Add(this.expression())
			Enddo
		Endif
		this.consume(TT_RPAREN, "Expect ')' after arguments")
		
		Return CreateObject("New", loKeyword, loClassName, loArguments)	
	EndFunc

	Hidden Function checkProperty(toProperty)
		If toProperty.Class = 'Literal'
			Return Type('toProperty.oValue') == 'C'
		Endif
		Return toProperty.Class = 'Variable'
	Endfunc

	Hidden Function getPropertyName(toProperty)
		If toProperty.Class = 'Literal'
			Return toProperty.oValue
		Endif
		Return toProperty.oName.lexeme
	Endfunc
	* ==========================================================

	Hidden Function match(tnType1, tnType2, tnType3)
		If This.Check(tnType1)
			This.advance()
			Return .T.
		Endif
		If !Empty(tnType2) And This.Check(tnType2)
			This.advance()
			Return .T.
		Endif
		If !Empty(tnType3) And This.Check(tnType3)
			This.advance()
			Return .T.
		Endif
		Return .F.
	Endfunc

	Hidden Function consume(tnType, tcErrorMsg)
		If This.Check(tnType)
			Return This.advance()
		Endif
		This.parseError(This.peek, tcErrorMsg)
	Endfunc

	Hidden Function Check(tnType)
		If This.isAtEnd
			Return .F.
		Endif
		Return This.peek.Type == tnType
	Endfunc

	Hidden Function advance
		If !This.isAtEnd
			This.nCurrent = This.nCurrent + 1
		Endif
		Return This.previous
	Endfunc

	Hidden Function isAtEnd_access
		Return This.peek.Type == TT_EOF
	Endfunc

	Hidden Function peek_access
		Return This.oTokens.Item(This.nCurrent)
	Endfunc

	Hidden Function peekNext(tnOffset)
		If (This.nCurrent + tnOffset) < This.oTokens.Count
			Return This.oTokens.Item(This.nCurrent+tnOffset).Type
		Endif
		Return TT_EOF
	Endfunc

	Hidden Function previous_access
		Return This.oTokens.Item(This.nCurrent-1)
	Endfunc

	Hidden Procedure parseError(toToken, tcErrorMessage)
		_Screen.SugarFox.errorToken(toToken, tcErrorMessage)
		_Screen.SugarFox.throwError('ParseError', 'token', toToken, tcErrorMessage)
	Endproc

	Hidden Function synchronize
		This.advance()
		Do While !This.isAtEnd
			If This.previous.Type == TT_SEMICOLON
				Return
			Endif
			If Inlist(This.peek.Type, ;
					TT_CLASS, ;
					TT_FUNCTION, ;
					TT_LOCAL, ;
					TT_PUBLIC, ;
					TT_PRIVATE, ;
					TT_FOR, ;
					TT_IF, ;
					TT_WHILE, ;
					TT_PRINT, ;
					TT_RETURN)
				Return
			Endif
			This.advance()
		Enddo
	Endfunc
Enddefine