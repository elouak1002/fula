name := "fula"
version := "1.0"
scalaVersion := "2.13.1"

mainClass in Compile := Some("Main")

assemblyJarName in assembly := "fulac.jar"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2" 
libraryDependencies += "org.typelevel" %% "cats-effect-kernel" % "3.1.0"
libraryDependencies += "org.typelevel" %% "cats-effect" % "3.1.0"
libraryDependencies += "org.ow2.asm" % "asm" % "9.1"