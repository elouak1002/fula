package frontend.typer;

import ast.FLType._;
import ast._;

object TypeChecker {
	
	def typeEqual(type1: FLType, type2: FLType): Either[String,FLType] = if (typeEqualBool(type1,type2) == true) if (type1 != FLUnknown) Right(type1) else Right(type2) else Left("Type error.")

	def typeEqualBool(type1: FLType, type2: FLType): Boolean = {
		if (type1 == type2) true else (type1,type2) match {
			case (FLUnknown,_) => true
			case (_,FLUnknown) => true
			case (FLFunc(_,_),FLWrap(typ)) if typ==type1 => true // Why true? Consider returning a def function FLFunc on a function that returns a FLWrap
			case (FLWrap(typ),FLFunc(_,_)) if typ==type2 => true // Why true? Consider returning a def function FLFunc on a function that returns a FLWrap --> Types are the same only the rep at runtime differ.
			case _ => false
		}
	}

	def typeEqualList(argsExpr: Seq[FLType], argsType: Seq[FLType]) : Either[String, Seq[FLType]] = (argsExpr,argsType) match {
		case (type1::xs1,type2::xs2) => if (typeEqualBool(type1,type2)) typeEqualList(xs1,xs2).map{ seq => type1+:seq } else Left("Type error in function call.")
		case (Nil,Nil) => Right(argsExpr)
		case (_,_) => Left("Type error in function call.")
	}

	def operationAllowedOnType(op: String, typ: FLType) : Either[String, FLType] = (op,typ) match {
		case ("+"|"-"|"*"|"%"|"/"|"<"|"<="|"=="|"!=",FLInt|FLFloat) => Right(typ)
		case ("==",FLString) => Right(typ)
		case ("&",FLInt) => Right(typ)
		case ("=="|"!=",FLBoolean) => Right(typ)
		case (_,_) => Left("Operator " + op + " not allowed on type " + typ + ".")
	}

}