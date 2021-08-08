package backend.codegen;

import cats.data.State;

import org.objectweb.asm._;
import org.objectweb.asm.ClassWriter._;
import org.objectweb.asm.Opcodes._;

import ast._;
import ast.TypeAst._;


object CompileProg {

	def compileProg(prog: TypeAst.TypeProg, filename: String) : Array[Byte] = {

		val cw: ClassWriter = new ClassWriter(COMPUTE_FRAMES + COMPUTE_MAXS);
		cw.visit(V1_8, ACC_PUBLIC + ACC_SUPER, "fula/"+filename,null, "java/lang/Object", null);
		val cw_mod = compileDecls(prog,cw)
		val bytes: Array[Byte] = cw_mod.toByteArray();
		bytes;
	}

	def compileDecls(prog: TypeAst.TypeProg, cw: ClassWriter) : ClassWriter = prog match {
		case decl::xs => {
			val cw_mod = CompileDecl.compileDecl(decl, cw)
			compileDecls(xs,cw_mod)
		}
		case Nil => cw
	}

}