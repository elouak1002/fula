package frontend.typer;

import struct._;
import struct.SymbolTable._;
import struct.Sym._;

import frontend.typer.TypeChecker._;

import ast.TypeAst._;
import ast.Ast._;
import ast.FLType._;
import ast._;

object DeclTyper {

	/**
	  * Resolve the argument list names of a function.
	  * Arguments are treated as declaration of values in the symbol table. (val arg: argType = ...)
	  */
	 val addArgDecls = (args: Seq[(String, FLType)], symT: SymbolTable) => SymbolTable.putMultipleSymbol(Sym.argsToSymbols(args),symT)

	 /**
	   * Resolve a declaration.
	   * @param prog
	   * @param symT
	   * @return either a string error or the root symbol table.
	   */
	   def typeFunctionBody(decl: Ast.Decl, symT: SymbolTable) : Either[String, TypeAst.TypeDecl] = decl match {
		case Ast.Decl.Def(name, pack, args, typ, body) => for {
			// Don't need to check the type of the args (as symbol entry has been created using it),
			// only resolve the name.
			argSymT <- addArgDecls(args,symT)
			tyBlock <- ExprTyper.typeBlock(body,argSymT)
			typeOfBlock = getBlockType(tyBlock) // type of a block is the type of the last expression of the block.
			declType <- typeEqual(typeOfBlock,typ)
		} yield (TypeAst.TypeDecl.TyDef(name, pack, args.map{case (name,typ) => (name, FLType.wrapTypeToFunction(typ))} , tyBlock ,  createFunctionType(args.map(_._2),declType) ))
		
		case Ast.Decl.Main(_,packMain,typ,body) => for {
			tyBlock <- ExprTyper.typeBlock(body,symT)
			typeOfBlock = getBlockType(tyBlock) // type of a block is the type of the last expression of the block.
			mainType <- typeEqual(typeOfBlock,getFunctionReturnType(typ))
		} yield (TypeAst.TypeDecl.TyMain(pack=packMain,body=tyBlock))
	 }

	 /**
	   * Resolve the list of functions (declarations Def and Main) recursiveley.
	   * @param prog
	   * @param symT
	   * @return
	   */
	 def typeFunctions(prog: Ast.Prog, symT: SymbolTable) : Either[String, TypeAst.TypeProg] = prog match {
		case decl::xs => for {
			func <- typeFunctionBody(decl,openScope(symT))
			funcs <- typeFunctions(xs,symT)
		} yield (func+:funcs)
		case Nil => Right(List())
	}
}