package backend.lifter;

import ast.TypeAst._;
import ast.FLType._;
import ast._;

object LiftExpr {

	def liftExpr(expr: TypeAst.TypeExpr): Set[FunctionBundle] = expr match {
		case TypeAst.TypeExpr.TyIf(b,e1,e2,_) => liftExpr(b)++liftBody(e1)++liftBody(e2)
		case TypeAst.TypeExpr.TyAssign(_,_,args,_) => liftBody(args)
		case TypeAst.TypeExpr.TyApply(_,args,_) => liftBody(args)
		case TypeAst.TypeExpr.TyAop(_,a1,a2,_) => liftExpr(a1)++liftExpr(a2)
		case TypeAst.TypeExpr.TypeBexp.TyBop(_,a1,a2,_,_) => liftExpr(a1)++liftExpr(a2)
		case TypeAst.TypeExpr.TyVal(_,e,_,_) => liftExpr(e)
		case TypeAst.TypeExpr.TyFunction(name,pack,typ) => Set(FunctionBundle(name,pack,typ))
		case _ => Set()
	}

	def liftBody(body: TypeAst.TypeBlock, acc: Set[FunctionBundle]=Set()): Set[FunctionBundle] = body match {
		case expr::xs => liftBody(xs, LiftExpr.liftExpr(expr)++acc)
		case Nil => acc
	}

}