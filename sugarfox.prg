* ================================================================================== *
* SugarFox scripting languaje.
* Version: 0.0.1
* Author: Irwin Rodríguez <rodriguez.irwin@gmail.com>
*
* A complete Visual Foxpro compiler writen in Visual Foxpro.
*
* Example:
* Do SugarFox with 'print("Hello world!")'
*
* ================================================================================== *

* ================================================================================== *
* SugarFox class
* ================================================================================== *
#ifndef CONSTANTS_LOADED
	#include "SugarFox.h"
#endif
Define Class SugarFox As Custom
	lHadError = .F.
	lHadRunTimeError = .F.
	Hidden oInterpreter
	oContext = .null.

	Function Init
		This.oInterpreter = Createobject("Interpreter")
	Endfunc

	Function Run(tcFileNameOrScript)
		If Empty(tcFileNameOrScript)
			This.runPrompt()
		Else
			Return This.runFileOrScript(tcFileNameOrScript)
		Endif
	Endfunc

	Function runFileOrScript(tcFileNameOrScript)
		If Lower(Right(tcFileNameOrScript, 6)) == '.sgfox'
			tcFileNameOrScript = Strconv(Filetostr(tcFileNameOrScript), 11)
		Endif
		Return This.Execute(tcFileNameOrScript)
	Endfunc

	Function Execute(tcSource)
		Local loScanner, loTokens, loParser, lcOutput, ;
		loResolver, loStatements, llPrintTokens, llPrintAST

		This.lHadError = .F.
		This.lHadRunTimeError = .F.
		
		loScanner = Createobject("Scanner", tcSource)
		loTokens = loScanner.scanTokens()
		llPrintTokens = .F.
		llPrintAST = .F.

		* <DEBUG>
		If llPrintTokens
			Clear
			For Each loToken In loTokens
				? loToken.toString()
			EndFor
			Return
		Endif
		* <DEBUG>
		loParser = Createobject("Parser", loTokens)
		loStatements = loParser.parse()

		* Stop if there was a syntax error.
		If This.lHadError
			Return .Null.
		Endif

		* <DEBUG>
		If llPrintAST
			Local loASTPrinter
			loASTPrinter = Createobject("ASTPrinter")
			? loASTPrinter.Print(loStatements)
			Return
		Endif
		* <DEBUG>
		lcOutput = This.oInterpreter.interpret(loStatements)
		* DEBUG
		clear
		* DEBUG		
		Return this.oInterpreter.cInitialization + CRLF + lcOutput + this.oInterpreter.cClassList + CRLF + this.oInterpreter.cFunctionList + CRLF + this.getHelperFunctions()
	Endfunc

	Function getHelperFunctions
		Local lcHelper
		Text to lcHelper noshow textmerge pretext 7
			* Array Factory
			Function NewSugarFoxArray(tvPar1, tvPar2, tvPar3, tvPar4, tvPar5, tvPar6, tvPar7, tvPar8, tvPar9, tvPar10, ;
								tvPar11, tvPar12, tvPar13, tvPar14, tvPar15, tvPar16, tvPar17, tvPar18, tvPar19, tvPar20, ;
								tvPar21, tvPar22, tvPar23, tvPar24, tvPar25, tvPar26, tvPar27, tvPar28, tvPar29, tvPar30, ;
								tvPar31, tvPar32, tvPar33, tvPar34, tvPar35, tvPar36, tvPar37, tvPar38, tvPar39, tvPar40, ;
								tvPar41, tvPar42, tvPar43, tvPar44, tvPar45, tvPar46, tvPar47, tvPar48, tvPar49, tvPar50, ;
								tvPar51, tvPar52, tvPar53, tvPar54, tvPar55, tvPar56, tvPar57, tvPar58, tvPar59, tvPar60, ;
								tvPar61, tvPar62, tvPar63, tvPar64, tvPar65, tvPar66, tvPar67, tvPar68, tvPar69, tvPar70, ;
								tvPar71, tvPar72, tvPar73, tvPar74, tvPar75, tvPar76, tvPar77, tvPar78, tvPar79, tvPar80, ;
								tvPar81, tvPar82, tvPar83, tvPar84, tvPar85, tvPar86, tvPar87, tvPar88, tvPar89, tvPar90, ;
								tvPar91, tvPar92, tvPar93, tvPar94, tvPar95, tvPar96, tvPar97, tvPar98, tvPar99, tvPar100)
				Local i, loArray
				loArray = CreateObject('Collection')
				
				For i = 1 to Pcount()
					loArray.Add(Evaluate("tvPar" + Alltrim(Str(i))))
				EndFor
				
				Return loArray
			EndFunc
			
			* Dictionary factory
			Function NewSugarFoxDictionary(tvPar1, tvPar2, tvPar3, tvPar4, tvPar5, tvPar6, tvPar7, tvPar8, tvPar9, tvPar10, ;
					tvPar11, tvPar12, tvPar13, tvPar14, tvPar15, tvPar16, tvPar17, tvPar18, tvPar19, tvPar20, ;
					tvPar21, tvPar22, tvPar23, tvPar24, tvPar25, tvPar26, tvPar27, tvPar28, tvPar29, tvPar30, ;
					tvPar31, tvPar32, tvPar33, tvPar34, tvPar35, tvPar36, tvPar37, tvPar38, tvPar39, tvPar40, ;
					tvPar41, tvPar42, tvPar43, tvPar44, tvPar45, tvPar46, tvPar47, tvPar48, tvPar49, tvPar50, ;
					tvPar51, tvPar52, tvPar53, tvPar54, tvPar55, tvPar56, tvPar57, tvPar58, tvPar59, tvPar60, ;
					tvPar61, tvPar62, tvPar63, tvPar64, tvPar65, tvPar66, tvPar67, tvPar68, tvPar69, tvPar70, ;
					tvPar71, tvPar72, tvPar73, tvPar74, tvPar75, tvPar76, tvPar77, tvPar78, tvPar79, tvPar80, ;
					tvPar81, tvPar82, tvPar83, tvPar84, tvPar85, tvPar86, tvPar87, tvPar88, tvPar89, tvPar90, ;
					tvPar91, tvPar92, tvPar93, tvPar94, tvPar95, tvPar96, tvPar97, tvPar98, tvPar99, tvPar100)
				If Mod(Pcount(), 2) = 1
					Error 'keys does not match values'
				EndIf

				Local i, loDict, loKey, loValue
				loDict = Createobject('Collection')
				For i = 1 To Pcount() Step 2
					If Type("tvPar" + Alltrim(Str(i))) != 'C'
						Error 'Invalid key'
					EndIf
					loKey = Evaluate("tvPar" + Alltrim(Str(i)))
					loValue = Evaluate("tvPar" + Alltrim(Str(i+1)))
					loDict.Add(loValue, loKey)
				Endfor
				Return loDict
			EndFunc
			* New Function Expression
			Function newSugarFoxFunctionExpr(tcName)
				Return CREATEOBJECT(tcName)
			EndFunc
			* Member Access
			Function sugarFoxMemberAccess(toObject, tvIndex)
				If Type('toObject') == 'O' and Type('toObject.baseclass') == 'C' and toObject.baseclass == 'Collection'
					If Type('tvIndex') == 'N'
						Return toObject.Item(tvIndex)
					endif
					Return toObject.Item(toObject.GetKey(tvIndex))
				EndIf
				* String notation
				If Type('toObject') == 'C'
					Return Substr(toObject, tvIndex, 1)
				EndIf
			EndFunc

			* SugarFoxExecuteFunction
			Function sugarFoxExecuteFunction(tcFunctionName, tcArgumentList)
				Local lcMacro				
				If Type('tcFunctionName') == 'O' and tcFunctionName.class == 'SugarFoxFunctionExpr'
					lcMacro = "tcFunctionName.execute(" + tcArgumentList + ")"
				Else
					lcMacro = "tcFunctionName(" + tcArgumentList + ")"
				EndIf
				&lcMacro
			endfunc

			* Baseclass SugarFoxFunctionExpr
			Define Class SugarFoxFunctionExpr as Custom
				
			EndDefine
		EndText
		Return lcHelper
	EndFunc
	
	Function errorLine(tnLine, tnCol, tcMessage)
		This.reportError(tnLine, tnCol, "", tcMessage)
	Endfunc

	Function reportError(tnLine, tnCol, tcWhere, tcMessage)
		Messagebox(This.formatError("Parsing", tnLine, tnCol, tcWhere, tcMessage), 16, "SugarFox Error")
		This.lHadError = .T.
	Endfunc

	Function errorToken(toToken, tcMessage)
		If toToken.Type == TT_EOF
			This.reportError(toToken.Line, toToken.Col, " at end", tcMessage)
		Else
			This.reportError(toToken.Line, toToken.Col, toToken.lexeme, tcMessage)
		Endif
	Endfunc

	Function runtimeError(toException)
		Messagebox(This.formatError("Runtime", toException.Token.Line, toException.Token.Col, toException.Token.lexeme, toException.Message), 16, "SugarFox Error")
		This.lHadRunTimeError = .T.
	Endfunc

	Function formatError(tcErrorStr, tnLine, tnCol, tcWhere, tcMessage)
		Return "[" + Alltrim(Str(tnLine)) + ":" + Alltrim(Str(tnCol)) + "] - " + tcErrorStr + " error near of `" + tcWhere + "`: " + tcMessage
	Endfunc

	Function throwError(tcType, tcPropertyName, tvPropertyValue, tcMessage)
		Local oExp
		Try
			Throw
		Catch To oExp
			=AddProperty(oExp, 'type', tcType)
			=AddProperty(oExp, tcPropertyName, tvPropertyValue)
			oExp.Message = tcMessage
			Throw
		Endtry
	Endfunc
	
Enddefine