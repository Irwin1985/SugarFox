* ================================================================================== *
* Assign
* ================================================================================== *
Define Class Assign As Custom
	oOperator = .Null.
	oName = .Null.
	oValue = .Null.
	cHash = ''

	Function Init(toOperator, toName, toValue)
		This.oOperator = toOperator
		This.oName = toName
		This.oValue = toValue
		This.cHash = Sys(2015)
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitAssignStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* Block
* ================================================================================== *
Define Class Block As Custom
	oStatements = .Null.
	Function Init(toStatements)
		This.oStatements = toStatements
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitBlockStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* Class
* ================================================================================== *
Define Class ClassNode As Custom
	oName = .Null.
	oSuperclass = .Null.
	oMethods = .Null.

	Function Init(toName, toSuperClass, toMethods)
		This.oName = toName
		This.oSuperclass = toSuperClass
		This.oMethods = toMethods
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitClassNodeStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* Expression
* ================================================================================== *
Define Class Expression As Custom
	oExpression = .Null.

	Function Init(toExpression)
		This.oExpression = toExpression
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitExpressionStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* ForNode
* ================================================================================== *
Define Class ForNode As Custom
	oKeyword = .null.
	oIndexOrKey = .null.
	oValue = .null.
	oStop = .null.
	oBody = .null.
	Function Init(toKeyword, toIndexOrKey, toValue, toStop, toBody)
		this.oKeyword = toKeyword
		this.oIndexOrKey = toIndexOrKey
		this.oValue = toValue
		this.oStop = toStop
		this.oBody = toBody
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitForNodeStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* FunctionNode
* ================================================================================== *
Define Class FunctionNode As Custom
	oName = .Null.
	oParams = .Null.
	oBody = .Null.

	Function Init(toName, toParams, toBody)
		This.oName = toName
		This.oParams = toParams
		This.oBody = toBody
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitFunctionNodeStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* Enum
* ================================================================================== *
Define Class Enum As Custom
	okeyword = .Null.
	oName = .Null.
	oElements = .Null.

	Function Init(toKeyword, toName, toElements)
		this.okeyword = toKeyword
		this.oName = toName
		this.oElements = toElements
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitEnumStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* Guard
* ================================================================================== *
Define Class Guard As Custom
	oKeyword = .Null.
	oCondition = .Null.
	oBody = .Null.

	Function Init(toKeyword, toCondition, toBody)
		this.oKeyword = toKeyword
		This.oCondition = toCondition
		This.oBody = toBody
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitGuardStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* IfNode
* ================================================================================== *
Define Class IfNode As Custom
	oCondition = .Null.
	oThenBranch = .Null.
	oElseBranch = .Null.

	Function Init(toCondition, toThenBranch, toElseBranch)
		This.oCondition = toCondition
		This.oThenBranch = toThenBranch
		This.oElseBranch = toElseBranch
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitIfNodeStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* Import
* ================================================================================== *
Define Class Import As Custom
	oKeyword = .null.
	oFileName = .Null.
	
	Function Init(toKeyword, toFileName)
		this.oKeyword = tokeyword
		this.oFileName = toFileName
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitImportStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* Print
* ================================================================================== *
Define Class Print As Custom
	oExpression = .Null.

	Function Init(toExpression)
		This.oExpression = toExpression
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitPrintStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* ReturnNode
* ================================================================================== *
Define Class ReturnNode As Custom
	oKeyword = .Null.
	oValue = .Null.

	Function Init(toKeyword, toValue)
		This.oKeyword = toKeyword
		This.oValue = toValue
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitReturnNodeStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* VariableDecl
* ================================================================================== *
Define Class VariableDecl As Custom
	oKeyword = .null.
	oName = .Null.
	oInitializer = .Null.

	Function Init(toKeyword, toName, toInitializer)
		This.oKeyword = toKeyword
		This.oName = toName
		This.oInitializer = toInitializer
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitVariableDeclStmt(This)
	Endfunc
Enddefine

* ================================================================================== *
* WhileNode
* ================================================================================== *
Define Class WhileNode As Custom
	oCondition = .Null.
	oBody = .Null.

	Function Init(toCondition, toBody)
		This.oCondition = toCondition
		This.oBody = toBody
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitWhileNodeStmt(This)
	Endfunc
EndDefine

* ================================================================================== *
* DoWhile
* ================================================================================== *
Define Class DoWhile As Custom
	oCondition = .Null.
	oBody = .Null.

	Function Init(toCondition, toBody)
		This.oCondition = toCondition
		This.oBody = toBody
	Endfunc

	Function Accept(toVisitor)
		Return toVisitor.visitDoWhileStmt(This)
	Endfunc
Enddefine