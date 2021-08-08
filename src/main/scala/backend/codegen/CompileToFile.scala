package backend.codegen;

import cats.effect._;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

object CompileToFile {

	def writeByteCode(bytes: Array[Byte], filename: String) : IO[Either[String,Unit]] = IO {
		try {
			val pth: Path = Paths.get("fula/" + filename + ".class");
			val direct: Path = Paths.get("fula/");
			Files.createDirectories(direct);
			Right(Files.write(pth, bytes));
		} catch {case e: Exception => Left("Error: cannot write file.")}
	}	

	def writeByteCode2(bytes: Array[Byte], filename: String) : IO[Either[Unit,Unit]] = IO {
		try {
			val pth: Path = Paths.get("fula/" + filename + ".class");
			Right(Files.write(pth, bytes));
		} catch {case e: Exception => Left(e.printStackTrace())}
	}	

}