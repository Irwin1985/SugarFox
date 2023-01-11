* ================================================================================== *
* Token Type constants
* ================================================================================== *
#Define CONSTANTS_LOADED		.T.
#Define CRLF					Chr(13) + Chr(10)
#Define TRUE					.T.
#Define false					.F.
#Define TT_EOF					0
#Define TT_IGNORE				1
#Define TT_SEMICOLON			2
#Define TT_RELATIONAL_OPERATOR	3
#Define TT_EQUALITY_OPERATOR	4
#Define TT_SIMPLE_ASSIGN		5
#Define TT_COMPLEX_ASSIGN		6
#Define TT_NUMBER				7
#Define TT_STRING				8
#Define TT_IDENTIFIER			9
#Define TT_LPAREN				10
#Define TT_RPAREN				11
#Define TT_LBRACKET				12
#Define TT_RBRACKET				13
#Define TT_LBRACE				14
#Define TT_RBRACE				15
#Define TT_DOT					16
#Define TT_COMMA				17
#Define TT_COLON				18
#Define TT_LOGICAL_AND			19
#Define TT_LOGICAL_OR			20
#Define TT_LOGICAL_NOT			21
#Define TT_TERM_OPERATOR		22
#Define TT_FACTOR_OPERATOR		23

* ================================================================================== *
* Keywords constants
* ================================================================================== *
#Define TT_CONST				100
#Define TT_DO					101
#Define TT_FALSE				102
#Define TT_PUBLIC				103
#Define TT_BREAK				104
#Define TT_CONTINUE				105
#Define TT_ELSE					106
#Define TT_IMPORT				107
#Define TT_NEW					108
#Define TT_PUBLIC				109
#Define TT_THIS					110
#Define TT_LOCAL				111
#Define TT_CASE					112
#Define TT_ENUM					113
#Define TT_FOR					114
#Define TT_IN					115
#Define TT_NULL					116
#Define TT_RETURN				117
#Define TT_THROW				118
#Define TT_CATCH				119
#Define TT_FUNCTION				120
#Define TT_END					121
#Define TT_TRY					122
#Define TT_WHILE				123
#Define TT_CLASS				124
#Define TT_IF					125
#Define TT_SWITCH				126
#Define TT_TRUE					127
#Define TT_PRINT				128
#Define TT_AS					129
#Define TT_PRIVATE				130
#Define TT_THISFORM				131
#Define TT_GUARD				132

* ================================================================================== *
* Token Category constants
* ================================================================================== *
#Define TC_LITERAL		1
#Define TC_IGNORABLE	2
#Define TC_KEYWORD		3
#Define TC_IDENTIFIER	4
#Define TC_ASSIGNMENT	5
#Define TC_GENERIC		6
#Define TC_UNARY		7
#Define TC_ASSIGN		8
#Define TC_LESS			9
#Define TC_LESS_EQ		10
#Define TC_GREATER		11
#Define TC_GREATER_EQ	12
#Define TC_EQUAL		13
#Define TC_BANG			14
#Define TC_NOT_EQ		15
#Define TC_PLUS			16
#Define TC_MINUS		17
#Define TC_MUL			18
#Define TC_DIV			19