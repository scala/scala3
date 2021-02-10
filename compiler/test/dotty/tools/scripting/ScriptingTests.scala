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

  def showScriptUnderTest(scriptFile: File): Unit =
    printf("===> test script name [%s]\n",scriptFile.getName)


  val argss: Map[String, Array[String]] = (
    for
      argFile <- testFiles
      if argFile.getName.endsWith(".args")
      name = argFile.getName.dropExtension
      scriptArgs = readLines(argFile).toArray
    yield name -> scriptArgs).toMap

  def scalaFilesWithArgs(extension: String) = (
    for
      scriptFile <- testFiles
      if scriptFile.getName.endsWith(extension)
      name = scriptFile.getName.dropExtension
      scriptArgs = argss.getOrElse(name, Array.empty[String])
    yield scriptFile -> scriptArgs).toList.sortBy { (file,args) => file.getName }

  @Test def scriptingDriverTests =

    for (scriptFile,scriptArgs) <- scalaFilesWithArgs(".scala") do
      showScriptUnderTest(scriptFile)
      val unexpectedJar = script2jar(scriptFile)
      unexpectedJar.delete

      sys.props("script.path") = scriptFile.absPath
      ScriptingDriver(
        compilerArgs = Array(
          "-classpath", TestConfiguration.basicClasspath
        ),
        scriptFile = scriptFile,
        scriptArgs = scriptArgs
      ).compileAndRun { (path:java.nio.file.Path,classpath:String, mainClass:String) =>
        printf("mainClass from ScriptingDriver: %s\n",mainClass)
      }
      assert(! unexpectedJar.exists, s"not expecting jar file: ${unexpectedJar.absPath}")

  @Test def scriptingMainTests =
    for (scriptFile,scriptArgs) <- scalaFilesWithArgs(".sc") do
      showScriptUnderTest(scriptFile)
      val unexpectedJar = script2jar(scriptFile)
      unexpectedJar.delete

      sys.props("script.path") = scriptFile.absPath
      val mainArgs: Array[String] = Array(
        "-classpath", TestConfiguration.basicClasspath.toString,
        "-script", scriptFile.toString,
      ) ++ scriptArgs

      Main.main(mainArgs)
      assert(! unexpectedJar.exists, s"not expecting jar file: ${unexpectedJar.absPath}")

  @Test def scriptingJarTest =
    for (scriptFile,scriptArgs) <- scalaFilesWithArgs(".sc") do
      showScriptUnderTest(scriptFile)
      val expectedJar = script2jar(scriptFile)
      expectedJar.delete

      sys.props("script.path") = scriptFile.absPath
      val mainArgs: Array[String] = Array(
        "-classpath", TestConfiguration.basicClasspath.toString,
        "-save",
        "-script", scriptFile.toString,
      ) ++ scriptArgs

      Main.main(mainArgs)

      printf("===> test script jar name [%s]\n",expectedJar.getName)
      assert(expectedJar.exists)

      import scala.sys.process._
      val cmd = Array("java",s"-Dscript.path=${scriptFile.getName}","-jar",expectedJar.absPath)
        ++ scriptArgs
      Process(cmd).lazyLines_!.foreach { println }

  extension(f: File){
    def absPath = f.getAbsolutePath.replace('\\','/')
  }
