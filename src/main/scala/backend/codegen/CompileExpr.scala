package backend.codegen;

import ast._;
import ast.FLType._;

import ast.TypeAst._;
import ast.TypeAst;

import org.objectweb.asm._;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.ClassWriter._;
import org.objectweb.asm.Opcodes._;
import org.objectweb.asm.Type;

import cats.data.State;

object CompileExpr {

	val MODULE_NAME = "fula/"

	val nameToAddress = (name: String) => name.drop(1).toInt

	def compileIf(expr: TypeAst.TypeExpr.TyIf, mv: MethodVisitor) : MethodVisitor = expr match {
		case TypeAst.TypeExpr.TyIf(bexp, b1, b2, typ) => {
			val mv_mod1 = compileBexp(bexp,mv)
			val labelFalse = new Label();
			val labelEnd = new Label();
			mv.visitJumpInsn(IFEQ, labelFalse);
			val mv_mod2 = compileBody(b1,mv_mod1);
			mv_mod2.visitJumpInsn(GOTO, labelEnd);
			mv.visitLabel(labelFalse);
			val mv_mod3 = compileBody(b2,mv_mod2);
			mv_mod3.visitLabel(labelEnd);
			mv_mod3
		}
	}

	def compileBexpOperator(op: String, typ: FLType, mv: MethodVisitor): MethodVisitor = {
		
		val labelFalse = new Label();
		val labelEnd = new Label();
		
		typ match {
			case FLInt|FLBoolean => mv.visitInsn(ISUB);
			case FLFloat => mv.visitInsn(FCMPL);
			case FLString => mv.visitMethodInsn(INVOKEVIRTUAL, "java/lang/Object", "equals", "(Ljava/lang/Object;)Z", false); mv.visitLdcInsn(1); mv.visitInsn(IXOR);
		}

		op match {
			case "==" => mv.visitJumpInsn(IFNE, labelFalse)
			case "!=" => mv.visitJumpInsn(IFEQ, labelFalse)
			case "<" => mv.visitJumpInsn(IFGE, labelFalse)
			case "<=" => mv.visitJumpInsn(IFGT, labelFalse)
		}
		
		mv.visitLdcInsn(1);
		mv.visitJumpInsn(GOTO, labelEnd);
		mv.visitLabel(labelFalse);
		mv.visitLdcInsn(0);
		mv.visitLabel(labelEnd);

		mv;
	}

	def compileBexp(bexp: TypeAst.TypeExpr.TypeBexp, mv: MethodVisitor) : MethodVisitor = bexp match {
		case TypeAst.TypeExpr.TypeBexp.TyBop(op, aexp1, aexp2, exprTyp, typ) =>
			val mv_mod1 = compileExpr(aexp1,mv) 
			val mv_mod2 = compileExpr(aexp2,mv_mod1)
			val mv_mod3 = compileBexpOperator(op,exprTyp, mv_mod2)
			mv_mod3
	}

	def compileAexpOperator(op: String, typ: FLType, mv: MethodVisitor): MethodVisitor = (op,typ) match {
		case ("+",FLInt) => {mv.visitInsn(IADD);mv}
		case ("-",FLInt) => {mv.visitInsn(ISUB);mv}
		case ("/",FLInt) => {mv.visitInsn(IDIV);mv}
		case ("%",FLInt) => {mv.visitInsn(IREM);mv}
		case ("*",FLInt) => {mv.visitInsn(IMUL);mv}
		case ("&",FLInt) => {mv.visitInsn(IAND);mv}
		case ("+",FLFloat) => {mv.visitInsn(FADD);mv}
		case ("-",FLFloat) => {mv.visitInsn(FSUB);mv}
		case ("/",FLFloat) => {mv.visitInsn(FDIV);mv}
		case ("%",FLFloat) => {mv.visitInsn(FREM);mv}
		case ("*",FLFloat) => {mv.visitInsn(FMUL);mv}
		case (_,_) => mv
	}
	
	def compileAop(expr: TypeAst.TypeExpr.TyAop, mv: MethodVisitor) : MethodVisitor = expr match {
		case TypeAst.TypeExpr.TyAop(op, aexp1, aexp2, typ) => {
			val mv_mod1 = compileExpr(aexp1,mv) 
			val mv_mod2 = compileExpr(aexp2,mv_mod1)
			val mv_mod3 = compileAexpOperator(op,typ, mv_mod2)
			mv_mod3
		}
	}


	def compileAssign(expr: TypeAst.TypeExpr.TyAssign, mv: MethodVisitor) : MethodVisitor = expr match {
		case TypeAst.TypeExpr.TyAssign(name, pack, args, typ) => {
			val mv_mod = compileBody(args,mv)
			val pack_name: String = if (pack.startsWith("scala/")) pack else MODULE_NAME+pack
			mv.visitMethodInsn(INVOKESTATIC, pack_name, name, CompileDecl.funcTypToString( FLFunc(args.map(expr => modifyArg(getNodeType(expr))),typ) ), false);
			mv;
		}
	}

	def modifyArg(typ: FLType): FLType = typ match {
		case FLFunc(_,_) => FLWrap(typ)
		case _ => typ
	}

	def compileApply(expr: TypeAst.TypeExpr.TyApply, mv: MethodVisitor) : MethodVisitor = expr match {
		case TypeAst.TypeExpr.TyApply(name, args, typ) => {
			mv.visitVarInsn(ALOAD,nameToAddress(name))
			val mv_mod = compileApplyArgs(args,mv)
			mv.visitMethodInsn(INVOKEINTERFACE, CodeGenHelper.callLambdaArity(FLFunc(args.map(expr => getNodeType(expr)),typ)), "apply", CodeGenHelper.getSamDescriptor(FLFunc(args.map(expr => getNodeType(expr)),typ)), true);
			unboxReturn(typ,mv)
			mv;
		}
	}

	def compileApplyArgs(args: TypeAst.TypeBlock, mv: MethodVisitor) : MethodVisitor = args match {
		case expr::xs => {
			val mv_mod = compileExpr(expr, mv)
			val mv_mod2 = boxArg(getNodeType(expr),mv_mod)
			compileApplyArgs(xs,mv_mod2)
		}
		case Nil => mv
	}

	def boxArg(typ: FLType, mv: MethodVisitor): MethodVisitor = typ match {
		case FLInt => { mv.visitMethodInsn(INVOKESTATIC, "scala/runtime/BoxesRunTime", "boxToInteger", CompileDecl.funcTypToString(FLFunc(Seq(FLInt),FLIntObject)), false); mv }
		case FLFloat => { mv.visitMethodInsn(INVOKESTATIC, "scala/runtime/BoxesRunTime", "boxToFloat", CompileDecl.funcTypToString(FLFunc(Seq(FLFloat),FLFloatObject)), false); mv } 
		case FLBoolean => { mv.visitMethodInsn(INVOKESTATIC, "scala/runtime/BoxesRunTime", "boxToBoolean", CompileDecl.funcTypToString(FLFunc(Seq(FLBoolean),FLBooleanObject)), false); mv }
		case _ => mv
	}

	def unboxReturn(typ: FLType, mv: MethodVisitor): MethodVisitor = typ match {
		case FLInt => { mv.visitMethodInsn(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToInt", CompileDecl.funcTypToString(FLFunc(Seq(FLObject),FLInt)), false); mv }
		case FLFloat => { mv.visitMethodInsn(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToFloat", CompileDecl.funcTypToString(FLFunc(Seq(FLObject),FLFloat)), false); mv } 
		case FLBoolean => { mv.visitMethodInsn(INVOKESTATIC, "scala/runtime/BoxesRunTime", "unboxToBoolean", CompileDecl.funcTypToString(FLFunc(Seq(FLObject),FLBoolean)), false); mv }
		case FLString => { mv.visitTypeInsn(CHECKCAST,"java/lang/String"); mv }
		case FLIO(FLUnit) => { mv.visitTypeInsn(CHECKCAST,"fula/helper$IO"); mv }
		case _ => mv
	}

	def compileFunction(expr: TypeAst.TypeExpr.TyFunction, mv: MethodVisitor) : MethodVisitor = expr match {
		case TypeAst.TypeExpr.TyFunction(name, pack, typ) => {

			val hdl1: Handle = new Handle(Opcodes.H_INVOKESTATIC,"java/lang/invoke/LambdaMetafactory","altMetafactory", "(Ljava/lang/invoke/MethodHandles$Lookup;Ljava/lang/String;Ljava/lang/invoke/MethodType;[Ljava/lang/Object;)Ljava/lang/invoke/CallSite;",false)

			val lambdaDescriptor: String = CodeGenHelper.getFunctionDescriptor(typ)

			val hdl2: Handle = new Handle(Opcodes.H_INVOKESTATIC,MODULE_NAME+pack,name+"$adapted", lambdaDescriptor,false)

			val samSignature = Type.getMethodType(CodeGenHelper.getSamDescriptor(typ))
			val instSignature = Type.getMethodType(lambdaDescriptor)

			val functionArity = CodeGenHelper.getFunctionArity(typ)

			mv.visitInvokeDynamicInsn("apply",functionArity,hdl1,samSignature,hdl2,instSignature,1);
			mv;
		}
	}

	def compileValue(expr: TypeAst.TypeExpr.TyValue, mv: MethodVisitor) : MethodVisitor = expr match {
		case TypeAst.TypeExpr.TyValue(str, typ) => typ match {
			case FLInt|FLBoolean => mv.visitVarInsn(ILOAD,nameToAddress(str))
			case FLFloat => mv.visitVarInsn(FLOAD,nameToAddress(str))
			case FLIO(_)|FLString|FLWrap(_)|FLObject => mv.visitVarInsn(ALOAD,nameToAddress(str))
		}
		mv; 
	}

	def compileVal(expr: TypeAst.TypeExpr.TyVal, mv: MethodVisitor) : MethodVisitor = expr match {
		case TypeAst.TypeExpr.TyVal(name, expr, exprTyp, typ) => exprTyp match {
			case FLInt|FLBoolean => {
				val mv_mod = compileExpr(expr,mv)
				mv_mod.visitVarInsn(ISTORE,nameToAddress(name))
				mv_mod
			}
			case FLFloat => {
				val mv_mod = compileExpr(expr,mv)
				mv_mod.visitVarInsn(FSTORE,nameToAddress(name))
				mv_mod
			}
			case FLIO(_)|FLString|FLWrap(_)|FLObject => {
				val mv_mod = compileExpr(expr,mv)
				mv_mod.visitVarInsn(ASTORE,nameToAddress(name))
				mv_mod
			}
		}
	}

	def compileExpr(expr: TypeAst.TypeExpr, mv: MethodVisitor) : MethodVisitor = expr match {
		case expr: TypeAst.TypeExpr.TyIf => compileIf(expr,mv)
		case expr: TypeAst.TypeExpr.TyAssign => compileAssign(expr,mv)
		case expr: TypeAst.TypeExpr.TyApply => compileApply(expr,mv)
		case expr: TypeAst.TypeExpr.TyFunction => compileFunction(expr,mv)
		case expr: TypeAst.TypeExpr.TyAop => compileAop(expr,mv)
		case expr: TypeAst.TypeExpr.TyValue => compileValue(expr,mv)
		case expr: TypeAst.TypeExpr.TyVal => compileVal(expr,mv)
		case expr: TypeAst.TypeExpr.TypeBexp.TyBop => compileBexp(expr,mv)
		case TypeAst.TypeExpr.TyFloatExpr(num,typ) => { mv.visitLdcInsn(num); mv}
		case TypeAst.TypeExpr.TyIntExpr(num,typ) => { mv.visitLdcInsn(num); mv}
		case TypeAst.TypeExpr.TyBooleanExpr(bool,typ) => { if (bool) mv.visitLdcInsn(1) else mv.visitLdcInsn(0); mv}
		case TypeAst.TypeExpr.TyStringExpr(str,typ) => { mv.visitLdcInsn(str); mv}
	}

	def compileBody(block: TypeAst.TypeBlock, mv: MethodVisitor) : MethodVisitor = block match {
		case expr::xs => {
			val mv_mod = compileExpr(expr, mv)
			compileBody(xs,mv_mod)
		}
		case Nil => mv
	}
}