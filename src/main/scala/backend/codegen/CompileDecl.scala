package backend.codegen;

import cats.data.State;

import org.objectweb.asm._;
import org.objectweb.asm.ClassWriter._;
import org.objectweb.asm.Opcodes._;

import ast._;
import ast.TypeAst._;


object CompileDecl {

	def funcTypToString(typ: FLType): String = typ match {
		case FLInt => "I"
		case FLIntObject => "Ljava/lang/Integer;"
		case FLFloat => "F" 
		case FLFloatObject => "Ljava/lang/Float;" 
		case FLUnit => "V"
		case FLBoolean => "Z"
		case FLBooleanObject => "Ljava/lang/Boolean;"
		case FLString => "Ljava/lang/String;"
		case FLIO(_) => "Lfula/helper$IO;"
		case FLObject => "Ljava/lang/Object;"
		case FLWrap(FLFunc(args,typ)) => "Lscala/Function" + args.length + ";"
		case FLFunc(args,typ) => "(" + args.map(funcTypToString(_)).mkString + ")" + funcTypToString(typ)
	}

	def compileReturn(typ: FLType) : Int = typ match {
		case FLInt => IRETURN
		case FLBoolean => IRETURN
		case FLFloat => FRETURN
		case FLUnit => RETURN
		case FLIO(_)|FLObject|FLWrap(_)|FLString => ARETURN
		case FLFunc(_,funcReturnTyp) => compileReturn(funcReturnTyp)
	}

	def funcSignature(typ: FLType) : String = typ match {
		case FLInt => "I"
		case FLIntObject => "Ljava/lang/Integer;"
		case FLFloat => "F" 
		case FLFloatObject => "Ljava/lang/Float;" 
		case FLUnit => "V"
		case FLBoolean => "Z"
		case FLBooleanObject => "Ljava/lang/Boolean;"
		case FLString => "Ljava/lang/String;"
		case FLIO(_) => "Lfula/helper$IO;"
		case FLObject => "Ljava/lang/Object;"
		case FLWrap(FLFunc(args,typ)) => "Lscala/Function" + args.length + "<" +  args.map(wrapSignature(_)).mkString + wrapSignature(typ) + ">" + ";"
		case FLFunc(args,typ) => "(" + args.map(funcSignature(_)).mkString + ")" + funcSignature(typ)
	}

	def wrapSignature(typ: FLType) : String = typ match {
		case FLInt|FLFloat|FLBoolean => "Ljava/lang/Object;"
		case _ => funcSignature(typ)
	}


	def compileDecl(decl: TypeAst.TypeDecl, cw: ClassWriter): ClassWriter = decl match {
		case TypeAst.TypeDecl.TyDef(name,_, args, body, typ) => {
			val mv: MethodVisitor = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, name, funcTypToString(typ), funcSignature(typ), null)
			mv.visitCode();
			val mv_mod = CompileExpr.compileBody(body, mv);
			mv_mod.visitInsn(compileReturn(typ));
			mv.visitMaxs(0, 0);
			mv_mod.visitEnd();
			cw;
		}
		case TypeAst.TypeDecl.TyMain(name, _, body, typ) => {
			val mv: MethodVisitor = cw.visitMethod(ACC_PUBLIC + ACC_STATIC, name, "([Ljava/lang/String;)V", null, null)
			mv.visitCode();
			val mv_mod = CompileExpr.compileBody(body, mv);
			mv.visitMethodInsn(INVOKEINTERFACE, "fula/helper$IO", "unsafeRunSync", "()V", true);
			mv.visitInsn(RETURN);
			mv.visitMaxs(0, 0);
			mv_mod.visitEnd();
			cw;
		}
	
	}

}