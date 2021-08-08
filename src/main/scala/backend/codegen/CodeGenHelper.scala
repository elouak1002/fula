package backend.codegen;

import org.objectweb.asm._;
import org.objectweb.asm.ClassWriter._;
import org.objectweb.asm.Opcodes._;

import ast._;
import ast.TypeAst._;

object CodeGenHelper {

	def getFunctionDescriptor(typ: FLType): String = typ match {
		case FLInt|FLFloat|FLBoolean => "Ljava/lang/Object;"
		case FLIntObject => "Ljava/lang/Integer;"
		case FLFloatObject => "Ljava/lang/Float;"
		case FLBooleanObject => "Ljava/lang/Boolean;"
		case FLString => "Ljava/lang/String;"
		case FLIO(_) => "Lfula/helper$IO;"
		case FLObject => "Ljava/lang/Object;"
		case FLFunc(args,typ) => "(" + args.map(getFunctionDescriptor(_)).mkString + ")" + getFunctionDescriptor(typ)
		case _ => ""
	}

	def getSamDescriptor(typ: FLType): String =  typ match {
		case FLFunc(args,typ) => "(" + args.map(getSamDescriptor(_)).mkString + ")" + getSamDescriptor(typ)
		case _ => "Ljava/lang/Object;"	
	}

	def getFunctionArity(typ: FLType): String =  typ match {
		case FLFunc(args,_) => "()Lscala/Function" + args.length + ";"
		case _ => ""
	}

	def callLambdaArity(typ: FLType): String =  typ match {
		case FLFunc(args,_) => "scala/Function" + args.length
		case _ => ""
	}

}