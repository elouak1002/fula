package ast;

trait FLType
case object FLObject extends FLType
case object FLUnknown extends FLType
case object FLInt extends FLType
case object FLIntObject extends FLType
case object FLFloat extends FLType
case object FLFloatObject extends FLType
case object FLBoolean extends FLType
case object FLBooleanObject extends FLType
case object FLUnit extends FLType
case object FLString extends FLType
case class FLIO(typ: FLType) extends FLType
case class FLWrap(typ: FLType) extends FLType
case class FLFunc(args: Seq[FLType], typ: FLType) extends FLType

object FLType {
	
	def createFunctionType(args: Seq[FLType],retType: FLType) : FLFunc = {
		val wrapArgs: Seq[FLType] = args.map(typ => wrapTypeToFunction(typ))
		val wrapRet: FLType = wrapTypeToFunction(retType)
		FLFunc(wrapArgs,wrapRet)
	}

	def createIOType(): FLType = FLIO(FLUnit)

	def createSingleType(typ: String): FLType = typ match {
		case "Int" => FLInt
		case "Float" => FLFloat
		case "Boolean" => FLBoolean
		case "String" => FLString
		case _ => createIOType()
	}

	def wrapTypeToFunction(typ: FLType) = typ match {
		case FLFunc(_,_) => FLWrap(typ)
		case _ => typ
	}

	def getFunctionArgTypes(typ: FLType) : Seq[FLType] = typ match {
		case FLWrap(FLFunc(args,typ)) => args
		case FLFunc(args,typ) => args
		case _ => Seq()
	}

	def getFunctionReturnType(typ: FLType) : FLType = typ match {
		case FLWrap(FLFunc(args,retType)) => retType
		case FLFunc(args,retType) => retType
		case _ => typ
	}

}