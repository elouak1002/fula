package frontend.parser;

import ast.Ast;
import fastparse._;
import fastparse.Parsed;

object ProgParser {

	 def parseProg(prog: String): Either[String, Ast.ParseProg] = fastparse.parse(prog, Fula.Prog(_)) match {
			case Parsed.Success(tree,_) => Right(tree)
			case Parsed.Failure(_,_,_) => Left("Syntax error.")
	}
}