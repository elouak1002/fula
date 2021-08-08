package frontend.parser;

import ast.Ast;
import ast._;
import ast.FLType._;
import fastparse._
import fastparse.MultiLineWhitespace._

/**
  * 
  */
object Expressions {
	
	// helpers for bexp
	def eq[_ : P] : P[Ast.Expr.Bexp] =  P(atom_bexp ~ "==" ~ atom_bexp).map{ case (x, z) => Ast.Expr.Bexp.Bop("==", x, z)} 
	def diff[_ : P] : P[Ast.Expr.Bexp] =  P(atom_bexp ~ "!=" ~ atom_bexp).map{ case (x, z) => Ast.Expr.Bexp.Bop("!=", x, z)} 
	def lt[_ : P] : P[Ast.Expr.Bexp] =  P(atom_bexp ~ "<" ~ atom_bexp).map{ case (x, z) => Ast.Expr.Bexp.Bop("<", x, z)} 
	def lte[_ : P] : P[Ast.Expr.Bexp] =  P(atom_bexp ~ "<=" ~ atom_bexp).map{ case (x, z) => Ast.Expr.Bexp.Bop("<=", x, z)} 
	def gt[_ : P] : P[Ast.Expr.Bexp] =  P(atom_bexp ~ ">" ~ atom_bexp).map{ case (x, z) => Ast.Expr.Bexp.Bop("<", z, x)} 
	def gte[_ : P] : P[Ast.Expr.Bexp] =  P(atom_bexp ~ ">=" ~ atom_bexp).map{ case (x, z) => Ast.Expr.Bexp.Bop("<=", z, x)}
	def bexp_paren[_ : P] : P[Ast.Expr.Bexp] = P( "(" ~ bexp ~ ")" )

	def atom_bexp[_ : P] : P[Ast.Expr] = P ( boolean | aexp | string_expr )

	// bexp
	def bexp[_ : P]: P[Ast.Expr.Bexp] = 	P( eq | diff | lt | lte | gt | gte | bexp_paren )

	// parsing of integers, double, boolean and values
	def int[_: P] : P[Ast.Expr.IntExpr] = P (Lexicals.Integer).map{ case( Ast.Tok.IntegerTok(num)) => Ast.Expr.IntExpr(num)}
	def double[_: P] : P[Ast.Expr.FloatExpr] = P (Lexicals.Float).map{ case( Ast.Tok.FloatTok(num)) => Ast.Expr.FloatExpr(num)}
	def boolean[_ : P] : P[Ast.Expr.BooleanExpr] = P(Lexicals.Boolean).map{ case( Ast.Tok.BooleanTok(bool)) => Ast.Expr.BooleanExpr(bool)}
	def value[_ : P] : P[Ast.Expr.Value] = P (Lexicals.Identifier).map{ case(Ast.Tok.Identifier(id)) => Ast.Expr.Value(id) }
	def string_expr[_ : P] : P[Ast.Expr.StringExpr] = P ( "\"" ~~ Lexicals.StringLex ~~ "\"" ).map{ case(Ast.Tok.StringTok(str)) => Ast.Expr.StringExpr(str) }
	
	//helpers for aexp
	def chainA[_: P](p: => P[Ast.Expr], op: => P[String]) = P( p ~ (op ~ p).rep ).map{case (lhs, rhs) =>rhs.foldLeft(lhs){case (lhs, (op, rhs)) =>Ast.Expr.Aop(op, lhs, rhs)}}
	def aexp_paren[_ : P] : P[Ast.Expr] = P (  "(" ~ aexp ~ ")" )
	def op[_ : P](op: String) = P (op).!

	// aexp 
	def aexp[_ : P] : P[Ast.Expr] = P (chainA(factor,op("+")|op("-")))
	def factor[_ : P] : P[Ast.Expr] = P (chainA(atom,op("*")|op("%")|op("/")|op("&")))
	def atom[_ : P] : P[Ast.Expr] = P ( assign_expr | aexp_paren | value | double | int )

	// Helper for chaining expr into a seq of expr
	def semi_chain[_ : P](p: => P[Ast.Expr]): P[Seq[Ast.Expr]] = P( p.rep(1,";"))

	// Helper for chaining argument of assign
	def comma_chain[_ : P](p: => P[Ast.Expr]): P[Seq[Ast.Expr]] = P( p.rep(0,",") )
	
	// if expression
	def if_expr[_ : P] : P[Ast.Expr.If] = P ( "if" ~ "(" ~ bexp ~ ")" ~ "{" ~ block ~ "}" ~ "else" ~ "{" ~  block ~ "}").map{
		case (bexp,if_seq,else_seq) => Ast.Expr.If(bexp,if_seq,else_seq)
	}

	// assign expression (calling a function)
	def assign_expr[_ : P]: P[Ast.Expr.Assign] = P ( Lexicals.Identifier ~ "(" ~ comma_chain(expr) ~ ")" ).map{
		case (Ast.Tok.Identifier(id),exprs) => Ast.Expr.Assign(id, exprs)
	}

	// println expression -> by default in the language.
	def writeln_expr[_ : P]: P[Ast.Expr] = P ("println" ~ "(" ~ expr ~ ")").map{ case (expr) => Ast.Expr.WriteLn(expr) }
	def write_expr[_ : P]: P[Ast.Expr] = P ("print" ~ "(" ~ expr ~ ")").map{ case (expr) => Ast.Expr.Write(expr) }

	def inside_join[_ : P] : P[Ast.Expr] = P (if_expr | writeln_expr | write_expr | assign_expr | value )
	def join[_ : P]: P[Ast.Expr] = P (inside_join ~ (">>" ~  inside_join).rep(1)).map{case (lhs, rhs) =>rhs.foldLeft(lhs){case (lhs, rhs) => Ast.Expr.Join(lhs, rhs)}}

	// assigning an expression to a value
	def val_expr[_ : P]: P[Ast.Expr.Val] = P ( "val" ~ Lexicals.Identifier ~ (":" ~ Lexicals.Type).? ~ "=" ~ expr ).map{ case(Ast.Tok.Identifier(id),Some(typ),expr) => Ast.Expr.Val(id,typ,expr) case(Ast.Tok.Identifier(id),None,expr) => Ast.Expr.Val(id,FLUnknown,expr) }

	def val_func[_ : P]: P[Ast.Expr.Val] = P ( "val" ~ Lexicals.Identifier ~ ":" ~ Lexicals.Type ~ "=" ~ expr ).map{ case(Ast.Tok.Identifier(id),typ,expr) => Ast.Expr.Val(id,typ,expr) }
	
	def lambda_expr[_: P]: P[Ast.Expr.Lambda] = P ( ArgsLambda ~ ":" ~ Lexicals.SingleTypes ~ "=>" ~ expr ).map{ case(args,typ,expr) => Ast.Expr.Lambda(args,typ,expr) }

	def ArgsLambda[_ : P]: P[Seq[(String,FLType)]] = P ("(" ~ (Lexicals.Identifier.map{ case(Ast.Tok.Identifier(id)) => id } ~ ":" ~ Lexicals.SingleTypes).rep(0,",") ~ ")")

	// An assignable to a singleton value expression
	def expr[_ : P]: P[Ast.Expr] = P ( if_expr | join | writeln_expr | write_expr | lambda_expr | bexp | aexp | assign_expr | string_expr | double | boolean | value | int )

	// An expression that can appear on a block
	def block_expr[_ : P]: P[Ast.Expr] = P ( val_expr | expr )

	// an expression block
	def block[_ : P]: P[Ast.Block] = P ( Expressions.semi_chain(Expressions.block_expr) ~ ";" | Expressions.block_expr.map{ case expr => List(expr) } ~ ";")

}