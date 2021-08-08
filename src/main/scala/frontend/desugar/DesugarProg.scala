package frontend.desugar;

import ast.TypeAst._;
import ast.FLType._;
import ast._;

case class DefList(wrapperName: String, current: Int, defs: Seq[Ast.Decl.Def]) {
	def addLambda(lambda: Ast.Expr.Lambda, className: String): (String,DefList) = {
		val newName: String = "$anonfun$" + wrapperName + "$" + current
		val newDefs: Seq[Ast.Decl.Def] = defs :+ Ast.Decl.Def(newName,className,lambda.args,lambda.typ,Seq(lambda.e))
		(newName,DefList(wrapperName,current+1,newDefs))
	}

	def getDefs(): Seq[Ast.Decl.Def] = defs
}

object DesugarProg {

	def desugarProg(prog: Ast.ParseProg, className: String) : Either[String,Ast.Prog] = prog match {
		case decl::xs => for {
			desugaredDecl <-  DesugarDecl.desugarDecl(decl, className)
			desugaredProg <-  desugarProg(xs, className)
		} yield (desugaredDecl:++desugaredProg)
		case Nil => Right(List())
	}
}