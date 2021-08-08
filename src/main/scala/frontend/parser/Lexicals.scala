package frontend.parser;

import ast.Ast;
import fastparse._
import fastparse.MultiLineWhitespace._

import ast._;
import ast.FLType._;

/**
  * 
  */
object Lexicals {

 	val keywordList: Set[String] = Set("if", "else", "def", "main", "val", "printlnFula", "True", "False", "printFula", "IO", "println", "print",  "join", "unsafeRunSync", "helper", "Int", "Float", "Boolean", "String", "IO[Unit]")

	 // Tokens
	def lowercase [_ : P] : P[String] = P( CharIn("a-z") ).!
	def uppercase[_ : P] : P[String] = P( CharIn("A-Z") ).!
	def letter[_ : P] : P[String]  = P( lowercase | uppercase ).!
	def digit [_ : P] : P[String]  = P( CharIn("0-9") ).!
	def unsignedIntegerStr[_ : P] : P[String] = P( digit.rep(1) ).!
	// Signed Integer to String parser
	def IntegerStr[_ : P] : P[String] = P ( "-".?.! ~ unsignedIntegerStr).!
	def BooleanStr[_ : P] : P[String] = P ("True" | "False").!

	// A boolean value
	def Boolean[_ : P] : P[Ast.Tok.BooleanTok] = P(BooleanStr).map(_.toBoolean).map{ case bool => Ast.Tok.BooleanTok(bool) }
	// An integer
	def Integer[_ : P] : P[Ast.Tok.IntegerTok] = P(IntegerStr).map(_.toInt).map{ case num => Ast.Tok.IntegerTok(num)}
	// A double
	def Float[_ : P] : P[Ast.Tok.FloatTok] = P (  IntegerStr ~ "." ~ unsignedIntegerStr ).!.map(_.toFloat).map{ case num => Ast.Tok.FloatTok(num)}

	def StringLex[_ : P] : P[Ast.Tok.StringTok] = P ((  letter | digit | "_" | "*" | " " | ".").repX   ).!.map{ case str => Ast.Tok.StringTok(str) }
	// A type

	def IOType[_ : P] : P[FLType] =  P( ("IO[Unit]")).!.map{ case (typ) => FLType.createIOType() }
	def SingleType[_ : P] : P[FLType] =  P( ("Int" | "Float" | "Boolean" | "String" )).!.map{ case (typ) => FLType.createSingleType(typ) } 
	def SingleTypes[_ : P] : P[FLType] = P ( SingleType | IOType )
	def FuncType[_ : P] : P[FLType] = P ( SingleTypes.rep(0,",") ~ "=>" ~ SingleTypes ).map{ case (seqTyp,typ) => FLFunc(seqTyp,typ) }
	def Type[_ : P] : P[FLType] =  P( FuncType | IOType | SingleType )

	// An identifier (name of function, name of value)
	def Identifier[_ : P]: P[Ast.Tok.Identifier] = P( letter ~ (letter | digit | "_").repX).!.filter(!keywordList.contains(_)).map{ case iden => Ast.Tok.Identifier(iden)}
	
}