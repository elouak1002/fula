package ast;

/**
  * 
  */
object Ast {

	sealed trait AstNode

	// Tokens used for preparsing (form of lexer)
	sealed trait Tok 
	object Tok {
		// Parsing
		case class Identifier(iden: String) extends Tok
		case class IntegerTok(num: Int) extends Tok
		case class FloatTok(num: Float) extends Tok
		case class BooleanTok(num: Boolean) extends Tok
		case class StringTok(str: String) extends Tok
		case class Type(typ: String) extends Tok {
			override def toString() = typ
		}
	}

	// A sequence of expression semi-column separated is a block
	type Block = Seq[Expr]

	sealed trait Expr extends AstNode
	object Expr {
		case class If(a: Bexp, e1: Block, e2: Block) extends Expr
		case class Assign(name: String, args: Seq[Expr]) extends Expr
		case class Value(s: String) extends Expr
		case class IntExpr(i: Int) extends Expr
		case class FloatExpr(d: Float) extends Expr
		case class BooleanExpr(b: Boolean) extends Expr
		case class StringExpr(str: String) extends Expr
		case class Aop(o: String, a1: Expr, a2: Expr) extends Expr
		case class Val(name: String, typ: FLType, e: Expr) extends Expr
		case class Lambda(args: Seq[(String,FLType)],typ: FLType, e: Expr) extends Expr
		
		// Only during parsing
		case class WriteLn(e: Expr) extends Expr
		case class Write(e: Expr) extends Expr
		case class Join(io1: Expr, io2: Expr) extends Expr

		// A boolean expression
		sealed trait Bexp extends Expr
		object Bexp {
			case class Bop(o: String, a1: Expr, a2: Expr) extends Bexp
		}
	}
	
	sealed trait Decl extends AstNode
	object Decl	{
		case class Def(name: String, pack: String, args: Seq[(String,FLType)], typ: FLType, body: Block) extends Decl
		case class Main(name: String="main", pack: String, typ: FLType=FLFunc(Seq(),FLIO(FLUnit)), body: Block) extends Decl
	}
	
	
	type ParseProg = Seq[AstNode]
	
	type Prog = Seq[Decl]
}