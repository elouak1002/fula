package backend.lifter;

import ast.TypeAst._;
import ast.FLType._;
import ast._;

object LiftDecl {

	def liftDecl(decl: TypeAst.TypeDecl): Set[FunctionBundle] = decl match {
		case TypeAst.TypeDecl.TyDef(_,_,_,body,_) => LiftExpr.liftBody(body)
		case TypeAst.TypeDecl.TyMain(_,_,body,_) => LiftExpr.liftBody(body)
	}

}