* ================================================================================== *
* Interpreter Class
* ================================================================================== *
#ifndef CONSTANTS_LOADED
	#include "SugarFox.h"
#endif

Define Class Interpreter As Custom
	oGlobals = .Null.
	oEnvironment = .Null.
	oBuiltins = .null.
	oLocals = .Null.
	oRegEx = .null.
	nDepth = 0	
	cFunctionList = ''
	cClassList = ''
	cInitialization = ''	
	
	Function Init
		with this
			.cInitialization = ''
			.cFunctionList = ''
			.cClassList = ''
			.oBuiltins = CreateObject("Dictionary")			
			.oRegEx = CreateObject("VBScript.RegExp")
			.oRegEx.IgnoreCase = .F.
			.oRegEx.Global = .T.
			
			.oGlobals = Createobject("Environment", .Null.)
			.oEnvironment = This.oGlobals
			.oLocals = Createobject("Dictionary")		
		EndWith
		* Register all builtins functions
		With this.oBuiltins
			.Put('clock', '0:<>:Seconds()')
			.Put('error', '2:<$MESSAGE,$TITLE_C_OPT>:MESSAGEBOX($MESSAGE, 16, $TITLE_C_OPT)')
			.Put('question', '2:<$MESSAGE,$TITLE_C_OPT>:MESSAGEBOX($MESSAGE, 36, $TITLE_C_OPT)')
			.Put('alert', '2:<$MESSAGE,$TITLE_C_OPT>:MESSAGEBOX($MESSAGE, 48, $TITLE_C_OPT)')
			.Put('info', '2:<$MESSAGE,$TITLE_C_OPT>:MESSAGEBOX($MESSAGE, 64, $TITLE_C_OPT)')
		EndWith
	Endfunc

	Function interpret(toStatements)
		Local lcOutput
		lcOutput = ''
		Try
			For Each loStatement In toStatements
				lcOutput = lcOutput + This.Execute(loStatement)
			Endfor
		Catch To loEx
			If Type('loEx.Type') == 'C'
				_Screen.SugarFox.runtimeError(loEx)
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
		Return lcOutput
	Endfunc

	Function Evaluate(toExpr)
		Return toExpr.Accept(This)
	Endfunc

	Function Execute(toStmt)
		Return toStmt.Accept(This)
	Endfunc

	Function resolve(toExpr, tnDepth)
		This.oLocals.put(toExpr.cHash, tnDepth)
	Endfunc

	Function executeBlock(toStatements)	
		Local loResult
		loResult = ''
		For Each loStatement In toStatements
			loResult = loResult + Replicate(Space(4), this.nDepth) + This.Execute(loStatement)
		Endfor
		Return loResult
	Endfunc

	Function visitArrayLiteralExpr(toExpr)
		Local lcOutput, i
		lcOutput = 'NewSugarFoxArray('
		i = 0
		For Each loElement In toExpr.oElements
			i = i + 1
			If i > 1
				lcOutput = lcOutput + ', '
			EndIf
			lcOutput = lcOutput + This.Evaluate(loElement)
		Endfor

		lcOutput = lcOutput + ')'
		Return lcOutput
	Endfunc

	Function visitArrayAccessExpr(toExpr)
		Local lcObject, lcIndex
		lcObject = this.evaluate(toExpr.oObject)
		lcIndex = this.evaluate(toExpr.oIndex)
		
		Return 'sugarFoxMemberAccess(' + lcObject + ',' + lcIndex + ')'
		
*!*			If this.getType(lcIndex) == 'N'
*!*				* Array Notation
*!*				Return lcObject + '.Item(' + lcIndex + ')'
*!*			EndIf
*!*			* Dictionary Notation
*!*			Return lcObject + '.Item(' + lcObject + '.GetKey(' + lcIndex + '))'
	EndFunc

	Function visitBlockStmt(toStmt)
		this.nDepth = this.nDepth + 1
		If Type('toStmt.oStatements') != 'O'
			Return ''
		EndIf
		Return This.executeBlock(toStmt.oStatements)
	Endfunc

	Function visitClassNodeStmt(toStmt)
		Local lcTemplate, lcClassName, lcClassBase, lcProperties, lcMethods, loMethod, i, loParam, loStmt
		Store '' TO lcTemplate, lcClassName, lcClassBase, lcProperties, lcMethods

		* Class Name
		lcClassName = toStmt.oName.lexeme
		
		* Class Base inheritance
		If !Isnull(toStmt.oSuperclass)
			lcClassBase = toStmt.oSuperclass.oName.lexeme
		Else
			lcClassBase = 'CUSTOM' && all base classes will inherit from Custom
		EndIf
		
		* Class methods
		For Each loMethod In toStmt.oMethods
			lcMethods = lcMethods + CRLF + 'FUNCTION ' + loMethod.oName.lexeme + '('
			i = 0
			For each loParam in loMethod.oParams
				i = i + 1
				If i > 1
					lcMethods = lcMethods + ', '
				EndIf
				lcMethods = lcMethods + loParam.lexeme
			EndFor
			lcMethods = lcMethods + ')'
			If loMethod.oName.lexeme == 'init'				
				For each loStmt in loMethod.oBody.oStatements
					if loStmt.Class == "Expression" and loStmt.oExpression.Class == "Set"
						lcProperties = lcProperties + loStmt.oExpression.oName.lexeme + ' = .null.' + CRLF
					EndIf
				EndFor
			EndIf
			lcMethods = lcMethods + CRLF + this.execute(loMethod.oBody) + CRLF
			lcMethods = lcMethods + 'ENDFUNC' + CRLF
		EndFor

		* Final template					
		Text to lcTemplate noshow pretext 7 textmerge
			Define Class <<lcClassName>> AS <<lcClassBase>>
				<<lcProperties>>
				<<lcMethods>>
			enddefine
		EndText
		this.cClassList = lcTemplate + CRLF + this.cClassList + CRLF
		Return ''		
		
*!*			LOCAL lcOutput, lcHeader, i, lcClassBody, lcProperties, lparseFields
*!*			lcProperties = ''
*!*			lcClassBody = ''
*!*			lcHeader = 'DEFINE CLASS ' + toStmt.oName.lexeme + ' AS '
*!*			If !Isnull(toStmt.oSuperclass)
*!*				lcHeader = lcHeader + toStmt.oSuperclass.oName		
*!*			Else
*!*				lcHeader = lcHeader + 'CUSTOM' && all base classes will inherit from Custom
*!*			Endif		

*!*			For Each loMethod In toStmt.oMethods
*!*				lcClassBody = lcClassBody + CRLF + 'FUNCTION ' + loMethod.oName.lexeme + '('
*!*				i = 0
*!*				For each loParam in loMethod.oParams
*!*					i = i + 1
*!*					If i > 1
*!*						lcClassBody = lcClassBody + ', '
*!*					EndIf
*!*					lcClassBody = lcClassBody + loParam.lexeme
*!*				EndFor
*!*				lcClassBody = lcClassBody + ')'
*!*				If loMethod.oName.lexeme == 'init'
*!*					For each loStmt in loMethod.oBody
*!*						if loStmt.Class == "Expression" and loStmt.oExpression.Class == "Set"
*!*							lcProperties = lcProperties + loStmt.oExpression.oName.lexeme + ' = .null.' + CRLF
*!*						EndIf
*!*					EndFor
*!*				EndIf
*!*				lcClassBody = lcClassBody + CRLF + this.executeBlock(loMethod.oBody) + CRLF
*!*				lcClassBody = lcClassBody + 'ENDFUNC' + CRLF
*!*			Endfor

*!*			lcOutput = lcHeader + CRLF + lcProperties + lcClassBody + CRLF + 'ENDDEFINE' + CRLF
*!*			this.cClassList = this.cClassList + lcOutput
*!*			
*!*			RETURN ''
	Endfunc

	Function visitExpressionStmt(toStmt)
		Return This.Evaluate(toStmt.oExpression) + CRLF
	Endfunc

	Function visitFunctionNodeStmt(toStmt)
		this.nDepth = this.nDepth + 1
		Local lcOutput, i
		lcOutput = 'FUNCTION ' + toStmt.oName.lexeme + '('
		i = 0
		For each loParam in toStmt.oParams
			i = i + 1
			If i > 1
				lcOutput = lcOutput + ', '
			EndIf
			lcOutput = lcOutput + loParam.lexeme
		EndFor
		lcOutput = lcOutput + ')'
		lcOutput = lcOutput + CRLF + this.executeBlock(toStmt.oBody.oStatements) + CRLF
		lcOutput = lcOutput + 'ENDFUNC'
		this.cFunctionList = lcOutput + CRLF + this.cFunctionList + CRLF
		
		RETURN ''
	Endfunc

	Function visitForNodeStmt(toStmt)
		Local lcOutput, loIndexOrKey, loValue, loStop, loBody
		lcOutput = ''
		loIndexOrKey = this.evaluate(toStmt.oIndexOrKey)
		loStop = this.evaluate(toStmt.oStop)
		loBody = This.Execute(toStmt.oBody)
		
		If !IsNull(toStmt.oValue) && for i,v style...
			loValue = this.evaluate(toStmt.oValue)
			Text to lcOutput noshow pretext 7 textmerge
				
				Local __sugarFoxStop, __sugarFoxCounter, <<loIndexOrKey>>, <<loValue>>
				__sugarFoxStop = <<loStop>>
				Do case
				case Type('__sugarFoxStop') == 'O' and Type('__sugarFoxStop.baseclass') == 'C' and __sugarFoxStop.baseclass == 'Collection'
					For __sugarFoxCounter = 1 to __sugarFoxStop.count
						<<loIndexOrKey>> = __sugarFoxStop.GetKey(__sugarFoxCounter)
						<<loIndexOrKey>> = Iif(Empty(<<loIndexOrKey>>), __sugarFoxCounter, <<loIndexOrKey>>)
						<<loValue>> = __sugarFoxStop.Item(__sugarFoxCounter)
						<<loBody>>
					EndFor
				Case Type('__sugarFoxStop') == 'N'
					For __sugarFoxCounter = 1 to __sugarFoxStop
						<<loIndexOrKey>> = __sugarFoxCounter						
						<<loValue>> = __sugarFoxCounter
						<<loBody>>
					EndFor
				Case Type('__sugarFoxStop') == 'C'
					For __sugarFoxCounter = 1 to Len(__sugarFoxStop)
						<<loIndexOrKey>> = __sugarFoxCounter
						<<loValue>> = Substr(__sugarFoxStop, __sugarFoxCounter, 1)
						<<loBody>>
					EndFor					
				endcase
			EndText
		Else
			* for i to j style
			Text to lcOutput noshow pretext 7 textmerge				
				Local __sugarFoxStop, __sugarFoxCounter, <<loIndexOrKey>>
				__sugarFoxStop = <<loStop>>
				Do case
				case Type('__sugarFoxStop') == 'O' and Type('__sugarFoxStop.baseclass') == 'C' and __sugarFoxStop.baseclass == 'Collection'
					For __sugarFoxCounter = 1 to __sugarFoxStop.count
						<<loIndexOrKey>> = __sugarFoxStop.Item(__sugarFoxCounter)
						<<loBody>>
					EndFor
				Case Type('__sugarFoxStop') == 'N'
					For __sugarFoxCounter = 1 to __sugarFoxStop
						<<loIndexOrKey>> = __sugarFoxCounter						
						<<loBody>>
					EndFor
				Case Type('__sugarFoxStop') == 'C'
					For __sugarFoxCounter = 1 to Len(__sugarFoxStop)
						<<loIndexOrKey>> = __sugarFoxCounter
						<<loBody>>
					EndFor					
				endcase
			EndText						
		EndIf
		Return lcOutput
	EndFunc

	Function visitEnumStmt(toStmt)
		Local lcOutput, lcTemplate, lcClassName, lcElements, i, lcKey, lcValue
		lcClassName = Sys(2015)
		lcElements = ''
		For i = 1 to toStmt.oElements.count
			lcKey = toStmt.oElements.GetKey(i)
			lcValue = Alltrim(Str(toStmt.oElements.Item(i)))
			lcElements = lcElements + lcKey + ' = ' + lcValue + CRLF
		EndFor
		
		Text to lcTemplate noshow pretext 7 textmerge
			DEFINE CLASS <<lcClassName>> AS CUSTOM
				<<lcElements>>				
			ENDDEFINE
		EndText
		this.cClassList = lcTemplate + CRLF + this.cClassList + CRLF
		
		Text to lcOutput noshow pretext 7 textmerge
			Local <<toStmt.oName.lexeme>>
			<<toStmt.oName.lexeme>> = CreateObject("<<lcClassName>>")
		EndText
		
		Return lcOutput + CRLF
	EndFunc

	Function visitGuardStmt(toStmt)
		Local lcTemplate
		Text to lcTemplate noshow pretext 7 textmerge
			Local _sugarFoxInternalBooleanVariable
			Try		
				_sugarFoxInternalBooleanVariable = <<this.evaluate(toStmt.oCondition)>>				
			Catch
				_sugarFoxInternalBooleanVariable = .f.
			EndTry
			If IsNull(_sugarFoxInternalBooleanVariable) or Empty(_sugarFoxInternalBooleanVariable)
				Release _sugarFoxInternalBooleanVariable
				<<this.execute(toStmt.oBody)>>				
			EndIf
			Release _sugarFoxInternalBooleanVariable
		endtext	
		Return lcTemplate + CRLF
	EndFunc

	Function visitIfNodeStmt(toStmt)
		Local lcOutput
		lcOutput = 'IF ' + This.Evaluate(toStmt.oCondition) + ' THEN' + CRLF
		lcOutput = lcOutput + This.Execute(toStmt.oThenBranch)
		If !Isnull(toStmt.oElseBranch.oStatements)
			lcOutput = lcOutput + 'ELSE' + CRLF
			lcOutput = lcOutput + This.Execute(toStmt.oElseBranch)
		EndIf
		lcOutput = lcOutput + 'ENDIF' + CRLF
		Return lcOutput
	EndFunc
	
	Function visitImportStmt(toStmt)
		Local lcFileName, lcContent, lcScript
		lcFileName = FullPath(toStmt.oFileName.lexeme + '.sgfox')
		If !File(lcFileName)
			This.runtimeError(toStmt.oKeyword, "File not found: '" + lcFileName + "'")
		EndIf
		lcContent = FileToStr(lcFileName)
		lcScript = _screen.sugarFox.run(lcContent)
		Return ''
	Endfunc	

	Function visitObjectLiteralExpr(toExpr)
		Local i, lcKey, loValue, lcOutput
		lcOutput = 'NewSugarFoxDictionary('
		For i=1 To toExpr.oElements.Count
			lcKey = toExpr.oElements.GetKey(i)
			loValue = This.Evaluate(toExpr.oElements.Item(i))
			If i == 1
				lcOutput = lcOutput + lcKey + ', ' + loValue
			Else
				lcOutput = lcOutput + ',' + lcKey + ', ' + loValue
			EndIf
		Endfor
		lcOutput = lcOutput + ')'
		Return lcOutput
	Endfunc

	Function visitPrintStmt(toStmt)
		Local loValue, lcOutput
		loValue = This.Evaluate(toStmt.oExpression)
		lcOutput = ''
		If Type('loValue') == 'O' and Type('loValue.Tag') == 'C' and loValue.Tag == 'Grouping'
			For each loVal in loValue
				lcOutput = lcOutput + CRLF + 'MESSAGEBOX(' + loVal + ')'
			EndFor
		Else			
			lcOutput = 'MESSAGEBOX(' + loValue + ')'
		EndIf
		Return lcOutput + CRLF
	Endfunc

	Function visitReturnNodeStmt(toStmt)
		Local loValue
		loValue = ''
		If !Isnull(toStmt.oValue)
			loValue = This.Evaluate(toStmt.oValue)
		Endif
		Return 'RETURN ' + loValue + CRLF
	Endfunc

	Function visitVariableDeclStmt(toStmt)
		Local lcValue, lcOutput					
		lcOutput = Upper(toStmt.oKeyword.lexeme) + Space(1) + toStmt.oName.lexeme + CRLF
		If !Isnull(toStmt.oInitializer)
			lcValue = This.Evaluate(toStmt.oInitializer)
			* Just register function expressions
			If 'newSugarFoxFunctionExpr'$lcValue
				this.oEnvironment.define(toStmt.oName.lexeme, lcValue)
			endif
			lcOutput = lcOutput + toStmt.oName.lexeme + ' = ' + lcValue + CRLF
		EndIf
		Return lcOutput
	Endfunc

	Function visitWhileNodeStmt(toStmt)
		return 'DO WHILE ' + This.Evaluate(toStmt.oCondition) + CRLF + This.Execute(toStmt.oBody) + 'ENDDO' + CRLF
	Endfunc

	Function visitDoWhileStmt(toStmt)
		Local lcOutput
		Text to lcOutput noshow pretext 7 textmerge
			Do while .T.
				<<This.Execute(toStmt.oBody)>>
				If <<This.Evaluate(toStmt.oCondition)>>
					Loop
				Else
					exit
				endif
			EndDo
		endtext
		Return lcOutput
	EndFunc

	Function visitAssignStmt(toExpr)
		Local loValue, lcOperator, lcLeft
		lcOperator = toExpr.oOperator.lexeme
		loValue = This.Evaluate(toExpr.oValue)
		lcLeft = toExpr.oName.lexeme

		If lcOperator == '='
			Return lcLeft + ' = ' + loValue + CRLF
		EndIf

		Return lcLeft + ' = ' + lcLeft + Left(lcOperator,1) + loValue + CRLF
	Endfunc

	Function visitBinaryExpr(toExpr)
		Local loLeft, loRight
		loLeft = This.Evaluate(toExpr.oLeft)
		loRight = This.Evaluate(toExpr.oRight)

		Do Case
		Case toExpr.oOperator.category == TC_NOT_EQ
			Return loLeft + ' != ' + loRight
		Case toExpr.oOperator.category == TC_EQUAL
			Return loLeft + ' == ' + loRight
		Case toExpr.oOperator.category == TC_GREATER
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft + ' > ' + loRight
		Case toExpr.oOperator.category == TC_GREATER_EQ
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft + ' >= ' + loRight
		Case toExpr.oOperator.category == TC_LESS
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft + ' < ' + loRight
		Case toExpr.oOperator.category == TC_LESS_EQ
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft + ' <= ' + loRight
		Case toExpr.oOperator.category == TC_MINUS
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft + ' - ' + loRight
		Case toExpr.oOperator.category == TC_PLUS
			Local lcLeftType, lcRightType
			lcLeftType = this.getType(loLeft)
			lcRightType = this.getType(loRight)
			If !InList(lcLeftType, 'C', 'N', 'W') and !InList(lcRightType, 'C', 'N', 'W')
				This.runtimeError(toExpr.oOperator, "Operands must be two numbers or two strings.")
			EndIf
			
			If lcLeftType == lcRightType
				Return loLeft + ' + ' + loRight
			EndIf
			If lcLeftType == 'C'
				Return loLeft + ' + TRANSFORM(' + loRight + ')'
			EndIf
			If lcLeftType == 'N' and lcRightType == 'C'
				Return loLeft + ' + VAL(' + loRight + ')'
			EndIf
			Return loLeft + ' + ' + loRight
			* This.runtimeError(toExpr.oOperator, "Operands must be two numbers or two strings.")
		Case toExpr.oOperator.category == TC_DIV
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft + ' / ' + loRight
		Case toExpr.oOperator.category == TC_MUL
			This.checkNumberOperands(toExpr.oOperator, loLeft, loRight)
			Return loLeft + ' * ' + loRight
		Endcase
		Return '.Null.'
	Endfunc

	Function visitCallExpr(toExpr)
		Local lcOutput, i, lcArguments
		lcOutput = This.Evaluate(toExpr.oCallee)		
		i = 0
		lcArguments = ''
		For Each loArgument In toExpr.oArguments
			i = i + 1
			If i > 1
				lcArguments = lcArguments + ','
			EndIf
			If loArgument.class == 'Variable'
				lcArguments = lcArguments + loArgument.oName.lexeme
			Else
				lcArguments = lcArguments + This.Evaluate(loArgument)
			endif
		EndFor

		If 'newSugarFoxFunctionExpr'$lcOutput
			Return toExpr.oCallee.oName.lexeme + '.execute(' + lcArguments + ')'
		Else
			If this.oBuiltins.ContainsKey(lcOutput) && Builtin functions
				Local lnArity, lcParam, lcParameters, lcFunctionName
				lcOutput = this.oBuiltins.Get(lcOutput)
				lnArity = Val(GetWordNum(lcOutput, 1, ':'))
				lcParameters = StrExtract(GetWordNum(lcOutput, 2, ':'), '<', '>')
				lcFunctionName = GetWordNum(lcOutput, 3, ':')
				If lnArity > 0
					If toExpr.oArguments.count > 0
						For i = 1 to lnArity
							lcParam = GetWordNum(lcParameters, i, ',')
							If i <= toExpr.oArguments.Count
								lcFunctionName = Strtran(lcFunctionName, lcParam, This.Evaluate(toExpr.oArguments.Item(i)))
							Else
								* Check if the parameter is optional
								If Right(lcParam, 3) == 'OPT'
									Local lcType
									lcType = GetWordNum(lcParam, 2, '_')
									DO CASE
									CASE lcType == 'C'
										lcFunctionName = Strtran(lcFunctionName, lcParam, '""')
									CASE lcType == 'N'
										lcFunctionName = Strtran(lcFunctionName, lcParam, "0")									
									OTHERWISE
									ENDCASE
								EndIf
							EndIf
						EndFor
					Else
						This.runtimeError(toExpr.oParen, "Too few arguments.")
					EndIf
				endif
				Return lcFunctionName
			EndIf
			Return lcOutput + '(' + lcArguments + ')' 
		EndIf
	Endfunc

	Function visitContextExpr(toExpr)
		Return This.oGlobals.Get(toExpr.oKeyword)
	Endfunc

	Function visitGetExpr(toExpr)
		Local loObject
		loObject = This.Evaluate(toExpr.oObject)
		Return loObject + '.' + toExpr.oName.lexeme
	Endfunc

	Function visitFunctionArrowExpr(toExpr)
		Local loFunction
		loFunction = Createobject("RuntimeFunction", toExpr, This.oEnvironment, false)
		If Type('loFunction') != 'O' Or loFunction.ParentClass != 'Callable'
			This.runtimeError(toExpr.oParen, "The current object is not a callable object.")
		Endif
		Return loFunction
	Endfunc

	Function visitFunctionExpr(toExpr)
		* FunctionExpr
		* var saludo = () => { return "Hola mundo"; };
		* // lo anterior se convierte a Fox:
		* DEFINE CLASS RandomName AS CUSTOM
		*	FUNCTION execute
		*		RETURN "Hola mundo"
		*	ENDFUNC
		* ENDDEFINE
		*		
		* 1. Crear clase y la instancia
		* 2. Asignar asignar el método.
		* Create the runtime function object and installs it in the current environment.
		Local lcClassDefinition, i, lcClassName
		lcClassName = Sys(2015)
		lcClassDefinition = 'DEFINE CLASS ' + lcClassName + ' AS SugarFoxFunctionExpr' + CRLF
		lcClassDefinition = lcClassDefinition + Space(4) + 'FUNCTION execute('
		i = 0
		* function parameters
		For each loParam in toExpr.oParams
			i = i + 1
			If i > 1
				lcClassDefinition = lcClassDefinition + ','
			EndIf
			lcClassDefinition = lcClassDefinition + loParam.lexeme
		EndFor
		lcClassDefinition = lcClassDefinition + ')' + CRLF
		* function body
		lcClassDefinition = lcClassDefinition + this.executeBlock(toExpr.oBody)
		lcClassDefinition = lcClassDefinition + Space(4) + 'ENDFUNC' + CRLF
		lcClassDefinition = lcClassDefinition + 'ENDDEFINE' + CRLF
		this.cClassList = lcClassDefinition + CRLF + this.cClassList + CRLF
		Return 'newSugarFoxFunctionExpr("' + lcClassName + '")'
	EndFunc
	
	Function visitGroupingExpr(toExpr)
		Local loResult
		loResult = CreateObject('Collection')
		loResult.tag = 'Grouping'
		For each loExpr in toExpr.oExpressions
			loResult.Add(This.Evaluate(loExpr))
		EndFor
		Return loResult
	Endfunc

	Function visitLiteralExpr(toExpr)
		Return this.stringify(toExpr.oValue)
	Endfunc

	Function visitLogicalExpr(toExpr)
		Local loLeft, loRight, lcOpe
		loLeft = This.Evaluate(toExpr.oLeft)
		loRight = this.Evaluate(toExpr.oRight)
		If toExpr.oOperator.lexeme == '||'
			lcOpe = ' OR '
		Else
			lcOpe = ' AND '
		EndIf
		Return loLeft + lcOpe + loRight	
	Endfunc

	Function visitNewExpr(toExpr)
		Local lcOutput
		lcOutput = 'CREATEOBJECT("' + toExpr.oClassName.lexeme + '"'
		For each loArg in toExpr.oArguments
			lcOutput = lcOutput + ', '
			lcOutput = lcOutput + this.Evaluate(loArg)
		EndFor
		lcOutput = lcOutput + ')'
		
		Return lcOutput
	Endfunc

	Function visitSetExpr(toExpr)
		Local loObject, loValue
		loObject = This.Evaluate(toExpr.oObject)		
		loValue = This.Evaluate(toExpr.oValue)
		Return loObject + '.' + toExpr.oName.lexeme + ' = ' + loValue + CRLF
	Endfunc

	Function visitThisNodeExpr(toExpr)
		Return 'THIS'
	Endfunc

	Function visitThisformNodeExpr(toExpr)
		Return 'THISFORM'
	Endfunc

	Function visitUnaryExpr(toExpr)
		Local lcRight
		lcRight = This.Evaluate(toExpr.oRight)
		Do Case
		Case toExpr.oOperator.category == TC_BANG
			Return '!' + lcRight
		Case toExpr.oOperator.category == TC_MINUS
			This.checkNumberOperand(toExpr.oOperator, lcRight)
			Return '-' + lcRight
		Case toExpr.oOperator.category == TC_PLUS
			This.checkNumberOperand(toExpr.oOperator, lcRight)
			Return lcRight
		Endcase

		Return '.Null.'
	Endfunc

	Function visitVariableExpr(toExpr)	
		Local lcValue
		lcValue = This.oEnvironment.Get(toExpr.oName)
*!*				Return '&' + toExpr.oName.lexeme
		If !Empty(lcValue)
			Return lcValue
		EndIf
		Return toExpr.oName.lexeme
	Endfunc

	Function lookUpVariable(toName, toExpr)
		Local lnDistance
		lnDistance = This.oLocals.Get(toExpr.cHash)
		If !Isnull(lnDistance)
			Return This.oEnvironment.getAt(lnDistance, toName.lexeme)
		Else
			Return This.oGlobals.Get(toName)
		Endif
	Endfunc

	Function getType(tcObject)
		DO CASE
		CASE this.isNumber(tcObject)
			Return 'N'
		CASE this.isString(tcObject)
			Return 'C'
		Case this.isIdent(tcObject)
			Return 'W'
		CASE this.isBoolean(tcObject)
			Return 'L'
		CASE this.isNull(tcObject)
			Return 'X'
		OTHERWISE
			Return 'U'
		ENDCASE
	EndFunc

	Function isNumber(tcObject)
		this.oRegEx.Pattern = '\d+'
		Return this.oRegEx.Test(tcObject)
	EndFunc
	
	Function isString(tcObject)
		Return InList(Left(tcObject,1), '"', "'") and InList(right(tcObject,1), '"', "'")
	EndFunc
	
	Function isBoolean(tcObject)
		Return InList(Lower(tcObject), '.f.', '.t.')
	EndFunc
	
	Function isNull(tcObject)
		Return Lower(tcObject) == '.null.'
	EndFunc	
	
	Function isIdent(tcObject)
		&& TODO(irwin): revisar el identifier.
		this.oRegEx.Pattern = '^[_a-zA-Z][_a-zA-Z0-9]*'
		Return this.oRegEx.Test(tcObject)
	EndFunc	

	Function checkNumberOperand(toOperator, tcOperand)
		If this.isNumber(tcOperand) or this.isIdent(tcOperand)
			Return
		Endif
		This.runtimeError(toOperator, "Operand must be a number.")
	Endfunc

	Function checkNumberOperands(toOperator, toLeft, toRight)
		If (this.isNumber(toLeft) or this.isIdent(toLeft)) and (this.isNumber(toRight) or this.isIdent(toRight))
			Return
		Endif
		This.runtimeError(toOperator, "Operands must be numbers.")
	Endfunc

	Function isTruthy(toObject)		
		If this.isNull(toObject)
			Return false
		EndIf
		
		If this.isBoolean(toObject)
			Return toObject
		Endif
		Return True
	Endfunc

	Function isEqual(toA, toB)
		If this.Isnull(toA) And this.Isnull(toB)
			Return TRUE
		Endif
		If this.Isnull(toA)
			Return false
		Endif
		Return toA == toB
	Endfunc

	Function stringify(toObject)
		Return Transform(toObject)
	Endfunc

	Hidden Procedure runtimeError(toToken, tcErrorMessage)
		*_Screen.sugarFox.errorToken(toToken, tcErrorMessage)
		_Screen.sugarFox.throwError('RuntimeError', 'token', toToken, tcErrorMessage)
	EndProc
	
Enddefine