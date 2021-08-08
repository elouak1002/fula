package frontend.desugar;

import ast.TypeAst._;
import ast.FLType._;
import ast._;

object DesugarExpr {

	def desugarArgsCall(argList: Seq[Ast.Expr], className: String, defList: DefList) : Either[String, (Seq[Ast.Expr],DefList)] = argList match {
		case arg::xs => for {
			desugaredArg <- desugarExpr(arg,className,defList)
			desugaredArgs <- desugarArgsCall(xs,className,desugaredArg._2)
		} yield (desugaredArg._1+:desugaredArgs._1,desugaredArgs._2)
		case Nil => Right(List(),defList)
	}

	def desugarExpr(expr: Ast.Expr,className: String, defList: DefList) : Either[String,(Ast.Expr,DefList)] = expr match {
		case Ast.Expr.Lambda(args,typ,e) => desugarExpr(e,className,defList).map(ret => {
			val liftedFunction: (String,DefList) = ret._2.addLambda(Ast.Expr.Lambda(args,typ,ret._1),className)
			(Ast.Expr.Value(liftedFunction._1),liftedFunction._2)
		})
		case Ast.Expr.Write(e1) => desugarExpr(e1,className,defList).map(de1 => (Ast.Expr.Assign("printFula", Seq(de1._1)),de1._2))
		case Ast.Expr.WriteLn(e1) => desugarExpr(e1,className,defList).map(de1 => (Ast.Expr.Assign("printlnFula", Seq(de1._1)),de1._2))
		case Ast.Expr.Join(e1,e2) => for {
			de1 <- desugarExpr(e1,className,defList)
			de2 <- desugarExpr(e2,className,de1._2)
		} yield (Ast.Expr.Assign("join",Seq(de1._1,de2._1)),de2._2)
		case Ast.Expr.Aop(op,e1,e2) => for {
			de1 <- desugarExpr(e1,className,defList)
			de2 <- desugarExpr(e2,className,de1._2)
		} yield (Ast.Expr.Aop(op,de1._1,de2._1),de2._2)
		case Ast.Expr.Bexp.Bop(op,e1,e2) => for {
			de1 <- desugarExpr(e1,className,defList)
			de2 <- desugarExpr(e2,className,de1._2)
		} yield (Ast.Expr.Bexp.Bop(op,de1._1,de2._1),de2._2)
		case Ast.Expr.If(bexp,b1,b2) => for {
			desugaredB1 <- desugarBody(b1,className,defList)
			desugaredB2 <- desugarBody(b2,className,desugaredB1._2)
		} yield (Ast.Expr.If(bexp,desugaredB1._1,desugaredB2._1),desugaredB2._2)
		case Ast.Expr.Assign(name,args) => for {
			desugaredArgs <- desugarArgsCall(args,className,defList)
		} yield (Ast.Expr.Assign(name,desugaredArgs._1),desugaredArgs._2)
		case _ => Right(expr,defList)
	}
	
	/**
	  * Remove expressions that will never be used i.e.
	  * expressions that are not assigned to a value or expressions
	  * that are not in the tail position of a block.
	  * @param body
	  * @return
	  */
	def desugarBody(body: Ast.Block, className: String, defList:DefList): Either[String,(Ast.Block,DefList)] = body match {
		case Ast.Expr.Val(name,typ,expr)::xs => for {
			desugaredExpr <- desugarExpr(expr,className,defList)
			desugaredBody <- desugarBody(xs,className,desugaredExpr._2)
		} yield (Ast.Expr.Val(name,typ,desugaredExpr._1)+:desugaredBody._1,desugaredBody._2)
		case expr::Nil => desugarExpr(expr,className,defList).map(exprD => (List(exprD._1),exprD._2))
		case expr::xs => desugarBody(xs,className,defList)
		case Nil => Right(List(),defList)
	}
}