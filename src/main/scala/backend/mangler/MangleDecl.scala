package backend.mangler;

import ast.TypeAst._;
import ast.FLType._;
import ast._;

import NameMapper._;

object MangleDecl {

	def mangleDeclArgs(args: Seq[(String,FLType)], nameMap: NameMapper): (List[(String,FLType)],NameMapper) = args match {
		case arg::xs => {
			val (nameMap2, argName) = nameMap.addName(arg._1)
			val argEntry: (String,FLType) = (argName,arg._2)
			val recurCall = mangleDeclArgs(xs,nameMap2)
			(argEntry+:recurCall._1,recurCall._2)
		}
		case Nil => (List(), nameMap)
	}

	def mangleDecl(decl: TypeAst.TypeDecl) : TypeAst.TypeDecl = decl match {
		case TypeAst.TypeDecl.TyDef(name, pack, args, body, typed)  => {
			val nameMap = NameMapper(Map(), None, 0)
			val (mangleedArgs, nameMap2) = mangleDeclArgs(args,nameMap)
			val mangleedBody: TypeAst.TypeBlock = MangleExpr.mangleDeclBody(body, nameMap2)
			TypeAst.TypeDecl.TyDef(name, pack, mangleedArgs, mangleedBody, typed)
		}
		case TypeAst.TypeDecl.TyMain(name, pack, body, typed)  => {
			val nameMap = NameMapper(Map(), None, 0)
			val mangleedBody: TypeAst.TypeBlock = MangleExpr.mangleDeclBody(body, nameMap)
			TypeAst.TypeDecl.TyMain(name, pack, mangleedBody, typed)
		}
	}
}