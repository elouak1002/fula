package struct;
import ast.Ast._;
import ast.FLType._;
import ast._;


case class Symbol(name: String, symTyp: FLType, pack: Option[String])
case class SymbolSignature(name: String, argsTyp: Option[Seq[FLType]])

object Sym {

	def createSymbolSignature(sym: Symbol) : SymbolSignature = {
		val symName: String = sym.name;
		val argsTyp: Option[Seq[FLType]] = sym.symTyp match {
			case FLFunc(args,typ) => Some(args)
			case _ => None
		}
		SymbolSignature(symName,argsTyp)
	}

	def createSymbolDecl(node: Ast.Decl) : Symbol = node match {
		case Decl.Def(name,pack,args,typ,_) => Symbol(name,createFunctionType(args.map(_._2),typ), Some(pack))
		case Decl.Main(name,pack,typ,_) => Symbol(name,FLFunc(Seq(),typ), Some(pack))
	}

	def createSymbolArg(node: (String,FLType)) : Symbol = {
		Symbol(node._1,wrapTypeToFunction(node._2), None)
	}

	val declToSymbols = (nodes: Seq[Ast.Decl]) => nodes.map(node => createSymbolDecl(node))
	val argsToSymbols = (nodes: Seq[(String,FLType)]) => nodes.map(arg => createSymbolArg(arg))

}