Set Procedure To "Expr" additive
Set procedure to "Interpreter" additive
Set Procedure To "Parser" additive
Set Procedure To "Runtime" additive
Set Procedure To "Scanner" ADDITIVE
Set Procedure To "Stmt" additive
Set Procedure To "Structures" additive
Set Procedure To "SugarFox" additive

If Type('_screen.SugarFox') == 'U'
	_screen.AddProperty('SugarFox', .null.)
EndIf
_screen.SugarFox = CreateObject("SugarFox")

Return