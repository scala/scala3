package dotty
package tools
package scripting

import java.io.File
import java.nio.file.Path

import org.junit.Test

import vulpix.TestConfiguration
import ScriptTestEnv.*

/** Runs all tests contained in `compiler/test-resources/scripting/` */
class ScriptingTests:
  // classpath tests managed by scripting.ClasspathTests.scala
  def testFiles = scripts("/scripting").filter { ! _.getName.toLowerCase.contains("classpath") }

  /*
   * Call .scala scripts without -save option, verify no jar created
   */
  @Test def scriptingDriverTests =
    for (scriptFile, scriptArgs) <- scalaFilesWithArgs(".scala") do
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
      ).compileAndRun { (path:java.nio.file.Path, classpathEntries:Seq[Path], mainClass:String) =>
        printf("mainClass from ScriptingDriver: %s\n", mainClass)
        true // call compiled script main method
      }
      assert( !unexpectedJar.exists, s"not expecting jar file: ${unexpectedJar.absPath}" )

  /*
   * Call .sc scripts without -save option, verify no jar created
   */
  @Test def scriptingMainTests =
    for (scriptFile, scriptArgs) <- scalaFilesWithArgs(".sc") do
      showScriptUnderTest(scriptFile)
      val unexpectedJar = script2jar(scriptFile)
      unexpectedJar.delete

      sys.props("script.path") = scriptFile.absPath
      val mainArgs: Array[String] = Array(
        "-classpath", TestConfiguration.basicClasspath.toString,
        "-script", scriptFile.toString,
      ) ++ scriptArgs

      Main.main(mainArgs)
      assert( !unexpectedJar.exists, s"not expecting jar file: ${unexpectedJar.absPath}")

  /*
   * Call .sc scripts with -save option, verify jar is created.
   */
  @Test def scriptingJarTest =
    for (scriptFile, scriptArgs) <- scalaFilesWithArgs(".sc") do
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

      printf("===> test script jar name [%s]\n", expectedJar.getName)
      assert(expectedJar.exists)

      callExecutableJar(scriptFile, expectedJar, scriptArgs)

  /*
   * Verify that when ScriptingDriver callback returns true, main is called.
   * Verify that when ScriptingDriver callback returns false, main is not called.
   */
  @Test def scriptCompileOnlyTests =
    val scriptFile = touchFileScript
    showScriptUnderTest(scriptFile)

    // verify main method not called when false is returned
    printf("testing script compile, with no call to script main method.\n")
    touchedFile.delete
    assert( !touchedFile.exists, s"unable to delete ${touchedFile}" )
    ScriptingDriver(
      compilerArgs = Array("-classpath", TestConfiguration.basicClasspath),
      scriptFile = scriptFile,
      scriptArgs = Array.empty[String]
    ).compileAndRun { (path:java.nio.file.Path, classpathEntries:Seq[Path], mainClass:String) =>
      printf("success: no call to main method in mainClass: %s\n", mainClass)
      false // no call to compiled script main method
    }
    touchedFile.delete
    assert( !touchedFile.exists, s"unable to delete ${touchedFile}" )

    // verify main method is called when true is returned
    printf("testing script compile, with call to script main method.\n")
    ScriptingDriver(
      compilerArgs = Array("-classpath", TestConfiguration.basicClasspath),
      scriptFile = scriptFile,
      scriptArgs = Array.empty[String]
    ).compileAndRun { (path:java.nio.file.Path, classpathEntries:Seq[Path], mainClass:String) =>
      printf("call main method in mainClass: %s\n", mainClass)
      true // call compiled script main method, create touchedFile
    }

    if touchedFile.exists then
      printf("success: script created file %s\n", touchedFile)
    if touchedFile.exists then printf("success: created file %s\n", touchedFile)
    assert( touchedFile.exists, s"expected to find file ${touchedFile}" )
   
  /*
   * Compile touchFile.sc to create executable jar, verify jar execution succeeds.
   */
  @Test def scriptingNoCompileJar =
    val scriptFile = touchFileScript
    showScriptUnderTest(scriptFile)
    val expectedJar = script2jar(scriptFile)
    sys.props("script.path") = scriptFile.absPath
    val mainArgs: Array[String] = Array(
      "-classpath", TestConfiguration.basicClasspath.toString,
      "-save",
      "-script", scriptFile.toString,
      "-compile-only"
    )

    expectedJar.delete
    Main.main(mainArgs) // create executable jar
    printf("===> test script jar name [%s]\n", expectedJar.getName)
    assert(expectedJar.exists, s"unable to create executable jar [$expectedJar]")

    touchedFile.delete
    assert( !touchedFile.exists, s"unable to delete ${touchedFile}" )
    printf("calling executable jar %s\n", expectedJar)
    callExecutableJar(scriptFile, expectedJar)
    if touchedFile.exists then
      printf("success: executable jar created file %s\n", touchedFile)
    assert( touchedFile.exists, s"expected to find file ${touchedFile}" )

///////////////////////////////////
  def touchFileScript = testFiles.find(_.getName == "touchFile.sc").get

  def touchedFile = File("touchedFile.out")

  def script2jar(scriptFile: File) = 
    val jarName = s"${scriptFile.getName.dropExtension}.jar"
    File(scriptFile.getParent, jarName)

  def showScriptUnderTest(scriptFile: File): Unit =
    printf("===> test script name [%s]\n", scriptFile.getName)

  def argss: Map[String, Array[String]] = (
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
    yield scriptFile -> scriptArgs).toList.sortBy { (file, args) => file.getName }

  def callExecutableJar(script: File, jar: File, scriptArgs: Array[String] = Array.empty[String]) = {
    import scala.sys.process._
    val cmd = Array("java", s"-Dscript.path=${script.getName}", "-jar", jar.absPath)
      ++ scriptArgs
    Process(cmd).lazyLines_!.foreach { println }
  }
