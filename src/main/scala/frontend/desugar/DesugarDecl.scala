package frontend.desugar;

import ast.Ast._;
import ast._;

object DesugarDecl {

	def desugarDecl(expr: Ast.AstNode, className: String) : Either[String,Seq[Ast.Decl]] = expr match {
		//Transform Val syntatic sugar into functions.
		case Ast.Expr.Val(name, typ, e) => DesugarExpr.desugarBody(Seq(e),className,DefList(name,0,Seq())).map(body => Ast.Decl.Def(name, className,Seq(), typ, body._1)+:body._2.getDefs)
		case Ast.Decl.Def(name,_, args, typ, body) => DesugarExpr.desugarBody(body,className,DefList(name,0,Seq())).map(body => Ast.Decl.Def(name, className, args, typ, body._1)+:body._2.getDefs)
		case Ast.Decl.Main(name,_, typ, body) => DesugarExpr.desugarBody(body,className,DefList(name,0,Seq())).map(body => body._2.getDefs:++Seq(Ast.Decl.Main(name, className, typ, body._1)))
	}
}