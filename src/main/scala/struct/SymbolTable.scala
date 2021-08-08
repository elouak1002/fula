package struct;

import struct.Sym._;
import ast._;

trait SymbolTable

case class Package(table: Map[SymbolSignature,Symbol]) extends SymbolTable
case class Root(table: Map[String, Symbol], pack: SymbolTable) extends SymbolTable
case class Node(table: Map[String, Symbol], parent: SymbolTable) extends SymbolTable

object SymbolTable {

	def getSymbol(symName: String, typCall: Option[Seq[FLType]], symT: SymbolTable) : Either[String, Symbol] = {
		lookup(SymbolSignature(symName,typCall),symT) match {
			case Some(Symbol(name,typ,pack)) => if (symName==name) Right(Symbol(name,typ,pack)) else Left("The value " + symName + " is not defined.")
			case None => {
				Left("The value " + symName + " is not defined.")
			}
		}
	}
	
	def putSymbol(symName: String, symType: FLType, packName: Option[String], symT: SymbolTable) : Either[String, SymbolTable] = {
		putSymbol(Symbol(symName, symType, packName),symT)
	}
	
	def putSymbol(symbol: Symbol, symTable: SymbolTable): Either[String,SymbolTable] = {
		if (alreadyDeclared(symbol,symTable,lookupScope)) {
			Left(errorLogging(symbol))
		} else {
			symTable match {
				case Package(map) => Right(Package(map + (createSymbolSignature(symbol) -> symbol)))
				case Root(map,pack) => Right(Root(map + (symbol.name -> symbol),pack))
				case Node(map, parent) => Right(Node(map + (symbol.name -> symbol),parent))
			}
		}
	}
	
	def putMultipleSymbol(symbols: Seq[Symbol], symT: SymbolTable): Either[String,SymbolTable] = symbols match {
		case symbol::xs => for {
			newSymT <- putSymbol(symbol,symT)
			symTab <- putMultipleSymbol(xs,newSymT)
		} yield (symTab)
		case Nil => Right(symT)
	}
	
	def errorLogging(sym: Symbol) : String = sym.symTyp match {
		case FLFunc(_,_) => "Function " + sym.name + " is already defined."
		case _ => "Value " + sym.name + " is already defined."
	}

	def openScope(symTable: SymbolTable) : SymbolTable = {
		Node(Map(),symTable)
	} 
	
	def alreadyDeclared(sym: Symbol, symTable: SymbolTable, f: (SymbolSignature, SymbolTable) => Option[Symbol]) : Boolean = {
		f(createSymbolSignature(sym),symTable).map(symbol => symbol.name==sym.name).getOrElse(false)
	}
	
	def lookup(symbol: SymbolSignature, symTable: SymbolTable) : Option[Symbol] = symTable match {
		case Package(table) => getFromMap(symbol,table)
		case Root(table,pack) => if (getFromMap(symbol.name,table) != None) getFromMap(symbol.name,table) else lookup(symbol,pack)
		case Node(table,parent) => if ( getFromMap(symbol.name,table) != None) getFromMap(symbol.name,table) else lookup(symbol,parent)
	}

	def lookupScope(symbol: SymbolSignature, symTable: SymbolTable) : Option[Symbol] = symTable match {
		case Package(table) => getFromMap(symbol,table)
		case Root(table,pack) => if (getFromMap(symbol.name,table) != None) getFromMap(symbol.name,table) else lookup(symbol,pack)
		case Node(table,parent) => getFromMap(symbol.name,table)
	}
	
	def getFromMap[A](symbol: A, table: Map[A,Symbol]) : Option[Symbol] = {
		try {
			Some(table(symbol))
		} catch {
			case e:Exception => None
		}
	}
}