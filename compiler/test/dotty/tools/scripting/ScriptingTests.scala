package dotty
package tools
package scripting

import java.io.File

import org.junit.Test

import vulpix.TestConfiguration


/** Runs all tests contained in `compiler/test-resources/scripting/` */
class ScriptingTests:
  extension (str: String) def dropExtension =
    str.reverse.dropWhile(_ != '.').drop(1).reverse

  def testFiles = scripts("/scripting")

  def script2jar(scriptFile: File) = 
    val jarName = s"${scriptFile.getName.dropExtension}.jar"
    File(scriptFile.getParent,jarName)

  val argss: Map[String, Array[String]] = (
    for
      argFile <- testFiles
      if argFile.getName.endsWith(".args")
      name = argFile.getName.dropExtension
      scriptArgs = readLines(argFile).toArray
    yield name -> scriptArgs).toMap

  @Test def scriptingDriverTests =

    for
      scriptFile <- testFiles
      if scriptFile.getName.endsWith(".scala")
      name = scriptFile.getName.dropExtension
      scriptArgs = argss.getOrElse(name, Array.empty[String])
    do
      val unexpectedJar = script2jar(scriptFile)
      unexpectedJar.delete

      ScriptingDriver(
        compilerArgs = Array(
          "-classpath", TestConfiguration.basicClasspath
        ),
        scriptFile = scriptFile,
        scriptArgs = scriptArgs
      ).compileAndRun { (path:java.nio.file.Path,classpath:String) =>
        path.toFile.listFiles.foreach { (f:File) => printf(" [%s]\n",f.getName) }
        printf("path: %s\nclasspath.length: %s\n",path,classpath.length)
      }
      printf("not expecting a jar file: %s\n",unexpectedJar.getName)
      assert(! unexpectedJar.exists )

  @Test def scriptingMainTests =

    for
      scriptFile <- testFiles
      if scriptFile.getName.endsWith(".scala")
      name = scriptFile.getName.dropExtension
      scriptArgs = argss.getOrElse(name, Array.empty[String])
    do
      val unexpectedJar = script2jar(scriptFile)
      unexpectedJar.delete

      sys.props("script.name") = scriptFile.getName
      val mainArgs: Array[String] = Array(
        "-classpath", TestConfiguration.basicClasspath.toString,
        "-script", scriptFile.toString,
      ) ++ scriptArgs

      Main.main(mainArgs)

      printf("not expecting a jar file: %s\n",unexpectedJar.getName)
      assert(! unexpectedJar.exists )

  @Test def scriptingJarTest =

    for
      scriptFile <- testFiles
      if scriptFile.getName.endsWith(".scala")
      name = scriptFile.getName.dropExtension
      scriptArgs = argss.getOrElse(name, Array.empty[String])
    do
      val expectedJar = script2jar(scriptFile)
      expectedJar.delete

      sys.props("script.name") = scriptFile.getName
      val mainArgs: Array[String] = Array(
        "-classpath", TestConfiguration.basicClasspath.toString,
        "-save",
        "-script", scriptFile.toString,
      ) ++ scriptArgs

      Main.main(mainArgs)

      printf("expected jar file: %s\n",expectedJar.getName)
      assert(expectedJar.exists)

      extension(f: File){
        def slashPath = f.getAbsolutePath.replace('\\','/')
      }
      import scala.sys.process._
      val cmd = Array("java",s"-Dscript.name=${scriptFile.getName}","-jar",expectedJar.slashPath)
        ++ scriptArgs
      Process(cmd).lazyLines_!.foreach { println }
