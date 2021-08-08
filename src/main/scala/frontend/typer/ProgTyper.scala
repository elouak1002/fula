package frontend.typer;

import ast.Ast._;
import ast._;
import struct._;

object ProgTyper {

	/**
	  * Resolve only the declaration of the functions, not the body.
	  */
	 val addFuncDecls = (nodes: Seq[Ast.Decl], symT: SymbolTable) => SymbolTable.putMultipleSymbol(Sym.declToSymbols(nodes),symT)
	 
	 val addPackage = (symbols: Seq[Symbol], symT: SymbolTable) => SymbolTable.putMultipleSymbol(symbols,symT)

	 /**
	   * Resolve an entire program semantic analysis.
	   * @param prog
	   * @return either a string error if the table can't pass the semantic analysis,
	   * or the sym table associated to the program.
	   */
	 def typeProg(prog: Ast.Prog): Either[String, TypeAst.TypeProg] = for {
			packSym <- addPackage(BuiltInPackage.fulaBuiltInPack,Package(Map()))
			val root: SymbolTable = Root(Map(),packSym)
			rootSym <- addFuncDecls(prog, root)
			typProg <- DeclTyper.typeFunctions(prog, rootSym)
		} yield (typProg)
}