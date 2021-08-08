import ast.Ast;
import ast.TypeAst;

import frontend.parser.ProgParser;
import frontend.desugar.DesugarProg;
import frontend.typer.ProgTyper;
import backend.mangler.MangleProg;
import backend.lifter.LiftProg;
import backend.codegen.CompileProg;
import backend.codegen.CompileToFile;

import cats.effect._;
import cats.data._;
import scala.io.Source;
import cats.effect.unsafe.implicits.global;

object Main {

	def readFile(filename: String) : IO[Either[String,String]] = IO {
		try {
		Right(scala.io.Source.fromFile(filename).mkString)
		} 
		catch {case e: Exception => Left("No file named " + filename + ".")}
	}

	def getFileName(args: Array[String]) : Either[String, String] = {
		try { 
			val filename = args(0)
			if (filename.endsWith(".fula")) Right(filename) else Left("Error: Must be fula file.")
		}
		catch {case e: Exception => Left("You must enter a filename.")}
	}

	def getTree(prog: String, className: String) : Either[String, TypeAst.TypeProg] = for {
		tree <- ProgParser.parseProg(prog)
		desugaredTree <- DesugarProg.desugarProg(tree, className)
		typeTree <- ProgTyper.typeProg(desugaredTree)
	} yield (typeTree)
	
	def compileTree(tree: TypeAst.TypeProg, filename: String): Array[Byte] = {
		val mangledTree = MangleProg.mangleProg(tree)
		val liftedTree = LiftProg.liftProg(mangledTree)
		CompileProg.compileProg(liftedTree, filename)
	}

	def main(args: Array[String]) : Unit = {

		val filename : Either[String,String] = getFileName(args)

		val className : Either[String, String] = filename.map(name => name.replaceAll(".+/","").stripSuffix(".fula"))

		val progStr : IO[Either[String,String]] = filename match {
			case Right(file) => readFile(file)
			case Left(error) => IO{Left(error)}
		}

		val progCompiled: IO[Either[String,Array[Byte]]] = progStr.map(x => for {
			prog <- x
			name <- className
			tree <- getTree(prog,name)
			jvmProg = compileTree(tree,name)
		} yield (jvmProg))


		val compileByteCode: EitherT[IO,String,Unit] = for {
			bytes <- EitherT(progCompiled)
			name <- EitherT(IO(className))
			_ <- EitherT(CompileToFile.writeByteCode(bytes, name))
		} yield ()

		compileByteCode.value.flatMap({
			case Right(io) => IO(io)
			case Left(error) => IO(println(error))
		}).unsafeRunSync

	}
}