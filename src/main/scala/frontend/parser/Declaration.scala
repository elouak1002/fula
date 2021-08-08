package frontend.parser;

import ast.Ast;
import ast._;
import ast.FLType._;
import fastparse._
import fastparse.MultiLineWhitespace._

/**
  * 
  */
object Declaration {

	def Args[_ : P]: P[Seq[(String,FLType)]] = P ((Lexicals.Identifier.map{ case(Ast.Tok.Identifier(id)) => id } ~ ":" ~ Lexicals.Type).rep(0,","))

	def Func[_: P]: P[Ast.Decl] = P( "def" ~ Lexicals.Identifier ~ "(" ~ Args ~ ")" ~ ":" ~ Lexicals.Type ~ "=" ~ "{" ~ Expressions.block ~ "}" ).map{
		case(Ast.Tok.Identifier(id),args,typ,block) => Ast.Decl.Def(id,"XXXX",args,typ,block)
	}
	
	def Main[_: P]: P[Ast.Decl] = P( "def" ~ "main" ~ "(" ~ ")" ~ ":" ~ "IO[Unit]" ~ "=" ~ "{" ~ Expressions.block ~ "}" ).map{
		case(block) => Ast.Decl.Main(pack="XXXX",body=block)
	}
}