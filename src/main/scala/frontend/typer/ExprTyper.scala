package frontend.typer;

import struct._;
import struct.SymbolTable._;
import struct.Sym._;

import frontend.typer.TypeChecker._;

import ast.TypeAst._;
import ast.Ast._;
import ast.FLType._;
import ast._;

object ExprTyper {

	def typeVal(valExpr: Ast.Expr.Val, symT: SymbolTable) : Either[String, (TypeAst.TypeExpr.TyVal,SymbolTable)] = valExpr match {
		case Ast.Expr.Val(name, typ, expr) => for {
			tyExpr <- typeExpr(expr,symT)
			valType <- typeEqual(typ,getNodeType(tyExpr))
			updatedType = FLType.wrapTypeToFunction(valType)
			newSymT <- SymbolTable.putSymbol(name, updatedType, None, symT)
		} yield (TypeAst.TypeExpr.TyVal(name,tyExpr,updatedType,FLUnit),newSymT)
	}

	def typeIf(ifExpr: Ast.Expr.If, symT: SymbolTable): Either[String, TypeAst.TypeExpr.TyIf] = ifExpr match {
		case Ast.Expr.If(bexp,block1,block2) => for {
			tyBexp <- typeBexp(bexp, symT) // typed bexp
			tyBlock1 <- typeBlock(block1, openScope(symT)) // typed first block
			tyBlock2 <- typeBlock(block2, openScope(symT)) // typed second block
			block1Type = getBlockType(tyBlock1) // type of block1 is the type of the last expression
			block2Type = getBlockType(tyBlock2) // type of block1 is the type of the last expression
			ifType <- typeEqual(block1Type,block2Type) // type of if expr
		} yield (TypeAst.TypeExpr.TyIf(tyBexp,tyBlock1,tyBlock2,ifType)) // typed if
	}

	def typeAssign(assignExpr: Ast.Expr.Assign, symT: SymbolTable): Either[String, TypeAst.TypeExpr] = assignExpr match {
		case Ast.Expr.Assign(name,args) => for {
			tyArgs <- typeBlock(args, symT) // type the args (args or not really a block but still a list of expr).
			exprTypes = tyArgs.map(tyExpr => getNodeType(tyExpr))
			funcSym <- SymbolTable.getSymbol(name,Some(exprTypes), symT) // type of function symbol with given name.
			argsTypes = FLType.getFunctionArgTypes(funcSym.symTyp)
			_ <- typeEqualList(exprTypes,argsTypes) // check the type of each argument.
			returnType = FLType.getFunctionReturnType(funcSym.symTyp)
			pack = funcSym.pack match { case Some(str) => str case None => "" }
			newTypeExpr = functionCallType(name,pack,tyArgs,funcSym.symTyp,returnType)
		} yield (newTypeExpr)
	}

	def functionCallType(name: String, pack: String, args: Seq[TypeExpr], funcTyp: FLType, retType: FLType): TypeAst.TypeExpr = funcTyp match {
		case FLWrap(_) => TypeAst.TypeExpr.TyApply(name,args,retType)
		case _ => TypeAst.TypeExpr.TyAssign(name,pack,args,retType)
	}

	def typeValue(valueExpr: Ast.Expr.Value, symT: SymbolTable): Either[String, TypeAst.TypeExpr] = valueExpr match {
		case Ast.Expr.Value(name) => for {
			valType <- SymbolTable.getSymbol(name,None,symT) // check if the name has already been defined
			newValueExpr = specialValue(valType)
		} yield(newValueExpr)
	}

	def specialValue(valSym: Symbol): TypeAst.TypeExpr = valSym.symTyp match {
		case FLFunc(_,_) => {
			val pack = valSym.pack match { case Some(str) => str case None => "" }
			TypeAst.TypeExpr.TyFunction(valSym.name, pack, valSym.symTyp)
		}
		case _ => TypeAst.TypeExpr.TyValue(valSym.name,FLType.wrapTypeToFunction(valSym.symTyp))
	}

	def typeAop(aopExpr: Ast.Expr.Aop, symT: SymbolTable): Either[String, TypeAst.TypeExpr.TyAop] = aopExpr match {
		case Ast.Expr.Aop(op,aexp1,aexp2) => for {
			tyAexp1 <- typeExpr(aexp1, symT) 
			tyAexp2 <- typeExpr(aexp2, symT)
			aopType <- typeEqual(getNodeType(tyAexp1),getNodeType(tyAexp2))
			opType <- TypeChecker.operationAllowedOnType(op, aopType)
		} yield (TypeAst.TypeExpr.TyAop(op,tyAexp1,tyAexp2,opType))
	}

	def typeExpr(expr: Ast.Expr, symT: SymbolTable) : Either[String, TypeAst.TypeExpr] = expr match {
		case expr: Ast.Expr.If => typeIf(expr,symT)
		case expr: Ast.Expr.Assign => typeAssign(expr,symT)
		case expr: Ast.Expr.Aop => typeAop(expr,symT)
		case expr: Ast.Expr.Value => typeValue(expr,symT)
		case expr: Ast.Expr.Bexp.Bop => typeBexp(expr,symT)
		case Ast.Expr.IntExpr(num) => Right(TypeAst.TypeExpr.TyIntExpr(num,FLInt)) 
		case Ast.Expr.FloatExpr(num) => Right(TypeAst.TypeExpr.TyFloatExpr(num,FLFloat))
		case Ast.Expr.BooleanExpr(bool) => Right(TypeAst.TypeExpr.TyBooleanExpr(bool,FLBoolean))
		case Ast.Expr.StringExpr(str) => Right(TypeAst.TypeExpr.TyStringExpr(str,FLString))
		case _ => {println(expr);Left("Syntax error.")}
	}

	def typeBexp(bexp: Ast.Expr.Bexp, symT: SymbolTable) : Either[String, TypeAst.TypeExpr.TypeBexp.TyBop] = bexp match {
		case Ast.Expr.Bexp.Bop(op,expr1,expr2) => for {
			tyExpr1 <- typeExpr(expr1,symT)
			tyExpr2 <- typeExpr(expr2,symT)
			bexpType <- typeEqual(getNodeType(tyExpr1),getNodeType(tyExpr2))
			opType <- TypeChecker.operationAllowedOnType(op, bexpType)
		} yield (TypeAst.TypeExpr.TypeBexp.TyBop(op,tyExpr1,tyExpr2,bexpType,FLBoolean))
	}

	def typeBlock(block: Ast.Block, symT: SymbolTable) : Either[String, TypeAst.TypeBlock] = block match {
		// Need to pass the symbol table along -> val is the only expr that update the symT by adding an entry
		case (valExpr:Ast.Expr.Val)::xs => typeVal(valExpr, symT).flatMap{
			case (tyVal, newSymT) => for { 
				tyBlock <- typeBlock(xs,newSymT)
			} yield (tyVal+:tyBlock)
		}
		// No need to pass the symT along, expr except val perform only read.
		case expr::xs => for {
			tyExpr <- typeExpr(expr, symT)
			tyBlock <- typeBlock(xs, symT)
		} yield (tyExpr+:tyBlock)
		case Nil => Right(List())
	}
}