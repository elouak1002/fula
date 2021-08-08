package backend.mangler;

import ast.TypeAst._;
import ast.FLType._;
import ast._;

object MangleProg {

	def mangleProg(prog: TypeAst.TypeProg) : TypeAst.TypeProg = prog.map(decl => MangleDecl.mangleDecl(decl))

}