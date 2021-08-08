package frontend.typer;

import struct._;
import ast._;

object BuiltInPackage {


	val printInt: Symbol = Symbol("printFula",FLFunc(Seq(FLInt),FLIO(FLUnit)),Some("helper"))
	val printlnInt: Symbol = Symbol("printlnFula",FLFunc(Seq(FLInt),FLIO(FLUnit)),Some("helper"))
	val printFloat: Symbol = Symbol("printFula",FLFunc(Seq(FLFloat),FLIO(FLUnit)),Some("helper"))
	val printlnFloat: Symbol = Symbol("printlnFula",FLFunc(Seq(FLFloat),FLIO(FLUnit)),Some("helper"))
	val printBoolean: Symbol = Symbol("printFula",FLFunc(Seq(FLBoolean),FLIO(FLUnit)),Some("helper"))
	val printlnBoolean: Symbol = Symbol("printlnFula",FLFunc(Seq(FLBoolean),FLIO(FLUnit)),Some("helper"))
	val printString: Symbol = Symbol("printFula",FLFunc(Seq(FLString),FLIO(FLUnit)),Some("helper"))
	val printlnString: Symbol = Symbol("printlnFula",FLFunc(Seq(FLString),FLIO(FLUnit)),Some("helper"))
	val join: Symbol = Symbol("join",FLFunc(Seq(FLIO(FLUnit),FLIO(FLUnit)),FLIO(FLUnit)),Some("helper"))

	val fulaBuiltInPack = Seq(printInt,printlnInt,printFloat,printlnFloat,printBoolean,printlnBoolean,printString,printlnString,join)

}