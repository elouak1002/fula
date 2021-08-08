package frontend.parser;

import ast.Ast;
import fastparse._
import fastparse.MultiLineWhitespace._
import java.beans.Expression

/**
  * 
  */
object Fula {
  
  def FuncDef[_ : P]: P[Ast.AstNode] = ( Declaration.Func | Expressions.val_func )

  def FuncList[_ : P]: P[Ast.ParseProg] = FuncDef.rep(1,";")

	def Prog[_ : P]: P[Ast.ParseProg] = P (  Declaration.Main.map{ case main => List(main)} ~ ";" | 
                                      (FuncList ~ ";" ~ Declaration.Main ~ ";").map{ case(funcs,main) => funcs :+ main}
                                    )
}