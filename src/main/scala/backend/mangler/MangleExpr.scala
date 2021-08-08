package backend.mangler;

import ast.TypeAst._;
import ast.FLType._;
import ast._;

object MangleExpr {

	def mangleIf(expr: TypeAst.TypeExpr.TyIf, nameMap: NameMapper) : (TypeAst.TypeExpr.TyIf,NameMapper) = expr match {
		case TypeAst.TypeExpr.TyIf(bexp, b1, b2, typ) => {
			val (dBexp,m0) = mangleBexp(bexp,nameMap)
			val (dB1,m1) = mangleBlock(b1,m0)
			val (dB2,m2) = mangleBlock(b2,m1)
			(TypeAst.TypeExpr.TyIf(dBexp, dB1, dB2,typ),m2)
		}
	}

	def mangleAssign(expr: TypeAst.TypeExpr.TyAssign, nameMap: NameMapper) : (TypeAst.TypeExpr.TyAssign,NameMapper) = expr match {
		case TypeAst.TypeExpr.TyAssign(name, pack, args, typ) => {
			val	(dArgs,m0) = mangleExprList(args,nameMap)
			(TypeAst.TypeExpr.TyAssign(name,pack,dArgs,typ),m0)
		}
	}

	def mangleApply(expr: TypeAst.TypeExpr.TyApply, nameMap: NameMapper) : (TypeAst.TypeExpr.TyApply,NameMapper) = expr match {
		case TypeAst.TypeExpr.TyApply(name, args, typ) => {
			val dName: String = nameMap.getName(name) match {
				case Some(str_new) => str_new
				case None => name
			}
			val	(dArgs,m0) = mangleExprList(args,nameMap)
			(TypeAst.TypeExpr.TyApply(dName,dArgs,typ),m0)
		}
	}

	def mangleAop(expr: TypeAst.TypeExpr.TyAop, nameMap: NameMapper) : (TypeAst.TypeExpr.TyAop,NameMapper) = expr match {
		case TypeAst.TypeExpr.TyAop(op, aexp1, aexp2, typ) => {
			val (dAexp1,m0) = mangleExpr(aexp1,nameMap)
			val (dAexp2,m1) = mangleExpr(aexp2,m0)
			(TypeAst.TypeExpr.TyAop(op,dAexp1,dAexp2,typ),m1)
		}
	}

	def mangleValue(expr: TypeAst.TypeExpr.TyValue, nameMap: NameMapper) : (TypeAst.TypeExpr.TyValue,NameMapper) = expr match {
		case TypeAst.TypeExpr.TyValue(str, typ) => {
			val dStr: String = nameMap.getName(str) match {
				case Some(str_new) => str_new
				case None => str
			}
			(TypeAst.TypeExpr.TyValue(dStr,typ),nameMap)
		}	
	}

	def mangleVal(expr: TypeAst.TypeExpr.TyVal, nameMap: NameMapper) : (TypeAst.TypeExpr.TyVal,NameMapper) = expr match {
		case TypeAst.TypeExpr.TyVal(name, expr, typExpr, typ) => {
			val (m0,dName) = nameMap.addName(name)
			val (dExpr,m1) = mangleExpr(expr,m0)
			(TypeAst.TypeExpr.TyVal(dName, dExpr,typExpr, typ),m1)
		}
	}

	def mangleBexp(bexp: TypeAst.TypeExpr.TypeBexp, nameMap: NameMapper) : (TypeAst.TypeExpr.TypeBexp.TyBop,NameMapper) = bexp match {
		case TypeAst.TypeExpr.TypeBexp.TyBop(op, aexp1, aexp2, exprTyp, typ) =>
			val (dAexp1,m0) = mangleExpr(aexp1,nameMap)
			val (dAexp2,m1) = mangleExpr(aexp2,m0)
			(TypeAst.TypeExpr.TypeBexp.TyBop(op, dAexp1, dAexp2,exprTyp,typ),m1)
	}


	def mangleExpr(expr: TypeAst.TypeExpr, nameMap: NameMapper) : (TypeAst.TypeExpr,NameMapper) = expr match {
		case expr: TypeAst.TypeExpr.TyIf => mangleIf(expr,nameMap)
		case expr: TypeAst.TypeExpr.TyAssign => mangleAssign(expr,nameMap)
		case expr: TypeAst.TypeExpr.TyApply => mangleApply(expr,nameMap)
		case expr: TypeAst.TypeExpr.TyAop => mangleAop(expr,nameMap)
		case expr: TypeAst.TypeExpr.TyValue => mangleValue(expr,nameMap)
		case expr: TypeAst.TypeExpr.TyVal => mangleVal(expr,nameMap)
		case expr: TypeAst.TypeExpr.TypeBexp.TyBop => mangleBexp(expr,nameMap)
		case _ => (expr,nameMap)
	}


	def mangleExprList(block: TypeAst.TypeBlock, nameMap: NameMapper) : (TypeAst.TypeBlock,NameMapper) = block match {
		case expr::xs => {
			val (mangleedExpr,exprMap) = mangleExpr(expr,nameMap)
			val recuCall = mangleExprList(xs,exprMap)
			(mangleedExpr+:recuCall._1,recuCall._2)
		}
		case Nil => (List(),nameMap)
	}

	def mangleBlock(block: TypeAst.TypeBlock, nameMap: NameMapper) : (TypeAst.TypeBlock,NameMapper) = {
		val blockMap = nameMap.openScope()
		val (mangleedBlock, blockMap2) = mangleExprList(block, blockMap)
		(mangleedBlock,blockMap2.closeScope().get)
	}

	def mangleDeclBody(block: TypeAst.TypeBlock, nameMap: NameMapper) : TypeAst.TypeBlock = {
		mangleBlock(block,nameMap)._1
	}
}