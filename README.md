# The fula compiler

Fula is a pure functional programming language. It is shipped with two jar files: a compiler and a runtime library.

### Support needed

The version of the software used must be the following ones :

- Java SE 16, can be downloaded from https://www.oracle.com/java/technologies/javase-jdk16-downloads.html 
- Scala 2.13.1
- Sbt 1.3.8

In order to install sbt use brew:

```properties
brew install sbt
```  

### Building the compiler

From the terminal, in the same directory where the build.sbt file is use the following command:

```properties
sbt assembly
```  

A jar file called fulac.jar is created in the target/scala-2.13 directory. It is the compiler jar file.

### Building the runtime library

If you are using macOS you can build the runtime library. In the runtime folder run:

```properties
sh build-library.sh
```  

The fular.jar file created in the runtime folder is the runtime library.

### Compiling a Fula file

From the terminal with the prebuild compiler use the following command:

```properties
java -jar fulac.jar filename.fula
```

It will create a class file name filename.class in a fula directory.

### Running a class file compiled with Fula

From the terminal use the following command to run a class file compiled with Fula using the prebuild fula runtime library:

If the file is in the same directory where the command is executed:

```properties
java -cp .:fular.jar fula.filename
```

Otherwise, give the path to the library, prebuild folder for instance:

```properties
java -cp .:./prebuild/fular.jar fula.filename
```
