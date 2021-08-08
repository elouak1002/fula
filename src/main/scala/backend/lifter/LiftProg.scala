package backend.lifter;

import ast.TypeAst._;
import ast.FLType._;
import ast._;

import org.objectweb.asm.Handle;
import org.objectweb.asm.Opcodes._;

case class FunctionBundle(name: String, pack: String, typ: FLType)

object LiftProg {

	def liftProg(prog: TypeAst.TypeProg) : TypeAst.TypeProg = {
		val liftDecls: Set[FunctionBundle] = getLiftedFunctions(prog)
		val adaptedDecls: Set[TypeAst.TypeDecl.TyDef] = liftDecls.map(fb => getAdaptedDecl(fb))
		val liftedProg: TypeAst.TypeProg =  adaptedDecls.toSeq++:prog
		liftedProg
	}

	def getLiftedFunctions(prog: TypeAst.TypeProg, acc: Set[FunctionBundle]=Set()): Set[FunctionBundle] = prog match {
		case decl::xs => getLiftedFunctions(xs,LiftDecl.liftDecl(decl)++acc)
		case Nil => acc
	}

	def getAdaptedDecl(fb: FunctionBundle) : TypeAst.TypeDecl.TyDef = {

		val funcArgsType: Seq[FLType] = getFunctionArgTypes(fb.typ)
		val returnType: FLType = getFunctionReturnType(fb.typ)
		val modifiedArgs: Seq[(String,FLType)] = modifyArgsType(funcArgsType)
 		TypeAst.TypeDecl.TyDef(fb.name+"$adapted",fb.pack,modifiedArgs,Seq(createBody(fb)),FLFunc(modifiedArgs.map(tup => tup._2),modifyReturnType(returnType)))
	}

	def modifyReturnType(typ: FLType) : FLType = typ match {
		case FLInt|FLBoolean|FLFloat => FLObject
		case _ => typ
	}

	def modifyArgsType(types: Seq[FLType], name: Int=0): Seq[(String,FLType)] = types match {
		case FLInt::xs => (("_"+name),FLObject) +: modifyArgsType(xs,name+1)
		case FLFloat::xs => (("_"+name),FLObject) +: modifyArgsType(xs,name+1)
		case FLBoolean::xs => (("_"+name),FLObject) +: modifyArgsType(xs,name+1)
		case FLString::xs => (("_"+name),FLString) +: modifyArgsType(xs,name+1)
		case FLIO(FLUnit)::xs => (("_"+name),FLIO(FLUnit)) +: modifyArgsType(xs,name+1)
		case _::xs => modifyArgsType(xs,name)
		case Nil => Seq()
	}

	def createBody(fb: FunctionBundle): TypeExpr = {
		val argsType: Seq[FLType] = FLType.getFunctionArgTypes(fb.typ)
		val returnType: FLType = FLType.getFunctionReturnType(fb.typ)

		val argsFunctionCall: Seq[TypeExpr] = processUnboxing(argsType)
		val functionCall: TypeExpr = processFunctionCall(fb.name, fb.pack, argsFunctionCall,returnType)
		processBoxing(functionCall,returnType)
	}

	def processUnboxing(types: Seq[FLType], name: Int=0): Seq[TypeExpr] = types match {
		case FLInt::xs =>  TypeAst.TypeExpr.TyAssign("unboxToInt","scala/runtime/BoxesRunTime",Seq(TypeAst.TypeExpr.TyValue("_"+name,FLObject)),FLInt)  +: processUnboxing(xs,name+1)
		case FLFloat::xs =>  TypeAst.TypeExpr.TyAssign("unboxToFloat","scala/runtime/BoxesRunTime",Seq(TypeAst.TypeExpr.TyValue("_"+name,FLObject)),FLFloat)  +: processUnboxing(xs,name+1)
		case FLBoolean::xs =>  TypeAst.TypeExpr.TyAssign("unboxToBoolean","scala/runtime/BoxesRunTime",Seq(TypeAst.TypeExpr.TyValue("_"+name,FLObject)),FLBoolean)  +: processUnboxing(xs,name+1)
		case FLString::xs => TypeAst.TypeExpr.TyValue("_"+name,FLString) +: processUnboxing(xs,name+1)
		case FLIO(FLUnit)::xs => TypeAst.TypeExpr.TyValue("_"+name,FLIO(FLUnit)) +: processUnboxing(xs,name+1)
		case _::xs => processUnboxing(xs,name)
		case Nil => Seq()
	}

	def processFunctionCall(name: String, pack: String, args: Seq[TypeExpr], typ: FLType): TypeExpr = {
		TypeAst.TypeExpr.TyAssign(name,pack,args,typ)
	}

	def processBoxing(funcCall: TypeExpr, returnType: FLType): TypeExpr = returnType match {
		case FLInt =>  TypeAst.TypeExpr.TyAssign("boxToInteger","scala/runtime/BoxesRunTime",Seq(funcCall),FLIntObject)
		case FLFloat =>  TypeAst.TypeExpr.TyAssign("boxToFloat","scala/runtime/BoxesRunTime",Seq(funcCall),FLFloatObject)
		case FLBoolean =>  TypeAst.TypeExpr.TyAssign("boxToBoolean","scala/runtime/BoxesRunTime",Seq(funcCall),FLBooleanObject)
		case _ => funcCall
	}

}