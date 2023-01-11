*!*	Set Step On
*!*	loSymTab = CreateObject("SymbolTable")

*!*	loNumberType = CreateObject("BuiltinTypeSymbol", "NUMBER")
*!*	loStringType = CreateObject("BuiltinTypeSymbol", "STRING")

*!*	* Define builtin types
*!*	loSymTab.Define(loNumberType)
*!*	loSymTab.Define(loStringType)

*!*	* Define a number type variable
*!*	loVarSymbol = CreateObject("VarSymbol", "x", loNumberType)

*!*	* Define a string type variable
*!*	loVarSymbol = CreateObject("VarSymbol", "y", loStringType)

*!*	Release all
* ================================================================================== *
* Dictionary Class (internal data structure)
* ================================================================================== *
Define Class Dictionary As Collection
	Function ContainsKey(tcKey)
		Local lnIndex
		If Empty(tcKey)
			Return .F.
		Endif
		Return This.GetKey(tcKey) > 0
	Endfunc

	Function put(tcKey, tvValue)
		If This.ContainsKey(tcKey)
			This.Remove(This.GetKey(tcKey))
		Endif
		This.Add(tvValue, tcKey)
	Endfunc

	Function Get(tvIndexOrKey) As Object
		Do Case
		Case Type('tvIndexOrKey') == 'N'
			If !Between(tvIndexOrKey, 1, This.Count)
				Return .Null.
			Endif
			Return This.Item(tvIndexOrKey)
		Case Type('tvIndexOrKey') == 'C'
			tvIndexOrKey = This.GetKey(tvIndexOrKey)
			If tvIndexOrKey > 0
				Return This.Item(tvIndexOrKey)
			Endif
		Endcase
		Return .Null.
	Endfunc

Enddefine

* ================================================================================== *
* Stack Class (internal data structure)
* ================================================================================== *
Define Class Stack As Collection
	Function Push
		Lparameters tvData As Variant
		Local lcType
		lcType = Type("tvData")
		If lcType != 'U'
			If lcType == 'C'
				Local lnIndex
				lnIndex = This.GetKey(tvData)
				If lnIndex > 0
					This.Remove(lnIndex)
				Endif
				This.Add(tvData, tvData) && same key for future search.
			Else
				This.Add(tvData, Sys(2015)) && random key (it doesn't matter)
			Endif
		Endif
	Endfunc

	Function Pop As Variant
		Local lvData As Variant

		If This.Count > 0
			lvData = This.Item(This.Count)
			This.Remove(This.Count)
		Else
			lvData = .Null.
		Endif
		Return lvData
	Endfunc

	Function peek As Variant
		Local lvData As Variant
		If This.Count > 0
			lvData = This.Item(This.Count)
		Else
			lvData = .Null.
		Endif
		Return lvData
	Endfunc

	Function Empty As Boolean
		Return This.Count == 0
	Endfunc

	Function Get(tvIndexOrKey) As Object
		Do Case
		Case Type('tvIndexOrKey') == 'N'
			If !Between(tvIndexOrKey, 1, This.Count)
				Return .Null.
			Endif
			Return This.Item(tvIndexOrKey)
		Case Type('tvIndexOrKey') == 'C'
			tvIndexOrKey = This.GetKey(tvIndexOrKey)
			If tvIndexOrKey > 0
				Return This.Item(tvIndexOrKey)
			Endif
		Endcase
		Return .Null.
	Endfunc

	Function Size As Integer
		Return This.Count
	Endfunc
EndDefine

* ================================================================================== *
* Symbol
* ================================================================================== *
Define Class Symbol as Custom
	cName = ''
	oType = .null.
	
	Function init(tcName, toType)
		this.cName = tcName
		If type('toType') == 'O'
			this.oType = toType
		EndIf
	EndFunc
enddefine

* ================================================================================== *
* SymbolTable
* ================================================================================== *
Define Class SymbolTable as Custom
	Hidden oSymbols
	oSymbols = .null.
	Function init
		this.oSymbols = CreateObject('Collection')
		this.installBuiltinTypes()
	EndFunc
	
	Hidden installBuiltinTypes
		this.define(CreateObject("Symbol", "BOOLEAN"))
		this.define(CreateObject("Symbol", "NUMBER"))
		this.define(CreateObject("Symbol", "STRING"))
		this.define(CreateObject("Symbol", "DATE"))
		this.define(CreateObject("Symbol", "DATETIME"))
		this.define(CreateObject("Symbol", "ARRAY"))
		this.define(CreateObject("Symbol", "HASH"))
	EndFunc
	
	Function define(toSymbol)
		this.oSymbols.Add(toSymbol, toSymbol.cName)
	EndFunc
	
	Function lookup(tcName)
		Local i
		i = this.oSymbols.GetKey(tcName)
		If Empty(i)
			Return .null.
		EndIf
		Return this.oSymbols.Item(i)
	EndFunc

EndDefine

* ================================================================================== *
* VarSymbol
* ================================================================================== *
Define Class VarSymbol as Symbol
	Function init(tcName, toType)
		DoDefault(tcName, toType)
	EndFunc
enddefine

* ================================================================================== *
* BuiltinTypeSymbol
* ================================================================================== *
Define Class BuiltinTypeSymbol as Symbol
	Function init(tcName)
		DoDefault(tcName)
	EndFunc
EndDefine