COBOL Analyzer
A static code analyzer for COBOL.

(C) 2009-2011 Alvise Spano'
	Universita' Ca' Foscari di Venezia
	
For information on Lintent please contact:
	spano at dais dot unive dot it.

COBOL Analyzer is a prototype of a parsing and typing system for COBOL.
It is still under active development is currently in alpha stage.
We may target a few updates in the future, when we plan to fix some minor
bugs.

A prototype of a UI is also present in the repository but that project
is no longer maintained: nonetheless it is capable of loading and showing
COBOL programs, while on the background a console window shows the output
of the analyzer as detailed log.


Currently supported features
	- island-parsing-like LALR parser
	- flow-type checking on variable reuse
	
Unsupported features or known bugs
	- abstract interpretation via polynomial domains
	- support of SQL constructs of COBOL85

