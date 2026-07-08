package dotty
package tools
package scripting

import org.junit.Test
import vulpix.TestConfiguration
import ScriptTestEnv.*
import dotty.tools.io.FileExtension
import dotty.tools.nio.File
import org.junit.Assume.assumeFalse

/** Runs all tests contained in `compiler/test-resources/scripting/` */
class ScriptingTests:
  // classpath tests managed by scripting.ClasspathTests.scala
  def testFiles = scripts("/scripting").filter { sc =>
    val name = sc.getName.toLowerCase
    !name.contains("classpath")
    && !name.contains("_scalacli")
  }

  /*
   * Call .scala scripts without -save option, verify no jar created
   */
  @Test def scriptingDriverTests =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    for (scriptFile, scriptArgs) <- scalaFilesWithArgs(".scala") do
      showScriptUnderTest(scriptFile.getName)
      val unexpectedJar = script2jar(scriptFile)
      unexpectedJar.delete()

      sys.props("script.path") = scriptFile.absPath
      ScriptingDriver(
        compilerArgs = Array(
          "-classpath", TestConfiguration.basicClasspath
        ),
        scriptFile = scriptFile,
        scriptArgs = scriptArgs
      ).compileAndRun { ctx ?=> (path, classpathEntries, mainClass) =>
        printf("mainClass from ScriptingDriver: %s\n", mainClass)
        true // call compiled script main method
      }
      assert(File.getOnDisk(unexpectedJar.path).isEmpty , s"not expecting jar file: ${unexpectedJar.path}" )

  /*
   * Call .sc scripts without -save option, verify no jar created
   */
  @Test def scriptingMainTests =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    for (scriptFile, scriptArgs) <- scalaFilesWithArgs(".sc") do
      showScriptUnderTest(scriptFile.getName)
      val unexpectedJar = script2jar(scriptFile)
      unexpectedJar.delete()

      sys.props("script.path") = scriptFile.absPath
      val mainArgs: Array[String] = Array(
        "-classpath", TestConfiguration.basicClasspath,
        "-script", scriptFile.toString,
      ) ++ scriptArgs

      Main.main(mainArgs)
      assert(File.getOnDisk(unexpectedJar.path).isEmpty, s"not expecting jar file: ${unexpectedJar.path}")

  /*
   * Call .sc scripts with -save option, verify jar is created.
   */
  @Test def scriptingJarTest =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    for (scriptFile, scriptArgs) <- scalaFilesWithArgs(".sc") do
      showScriptUnderTest(scriptFile.getName)
      val expectedJar = script2jar(scriptFile)
      expectedJar.delete()

      sys.props("script.path") = scriptFile.absPath
      val mainArgs: Array[String] = Array(
        "-classpath", TestConfiguration.basicClasspath,
        "-save",
        "-script", scriptFile.toString,
      ) ++ scriptArgs

      Main.main(mainArgs)

      printf("===> test script jar name [%s]\n", expectedJar.name)
      assert(File.getOnDisk(expectedJar.path).nonEmpty)

      callExecutableJar(scriptFile, expectedJar, scriptArgs)

  /*
   * Verify that when ScriptingDriver callback returns true, main is called.
   * Verify that when ScriptingDriver callback returns false, main is not called.
   */
  @Test def scriptCompileOnlyTests =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    val scriptFile = touchFileScript
    showScriptUnderTest(scriptFile.getName)

    // verify main method not called when false is returned
    printf("testing script compile, with no call to script main method.\n")
    File.getOnDisk(touchedFileName).foreach(_.delete())
    ScriptingDriver(
      compilerArgs = Array("-classpath", TestConfiguration.basicClasspath),
      scriptFile = scriptFile,
      scriptArgs = Array.empty[String]
    ).compileAndRun { ctx ?=> (path, classpathEntries, mainClass) =>
      printf("success: no call to main method in mainClass: %s\n", mainClass)
      false // no call to compiled script main method
    }
    File.getOnDisk(touchedFileName).foreach(_.delete())

    // verify main method is called when true is returned
    printf("testing script compile, with call to script main method.\n")
    ScriptingDriver(
      compilerArgs = Array("-classpath", TestConfiguration.basicClasspath),
      scriptFile = scriptFile,
      scriptArgs = Array.empty[String]
    ).compileAndRun { ctx ?=> (path, classpathEntries, mainClass) =>
      printf("call main method in mainClass: %s\n", mainClass)
      true // call compiled script main method, create touchedFile
    }

    File.getOnDisk(touchedFileName) match
      case Some(f) =>
        printf("success: script created file %s\n", f.path)
      case None =>
        throw new AssertionError(s"expected to find file ${touchedFileName}" )

  /*
   * Compile touchFile.sc to create executable jar, verify jar execution succeeds.
   */
  @Test def scriptingNoCompileJar: Unit =
    assumeFalse("Scripts do not yet support Scala 2 library TASTy", Properties.usingScalaLibraryTasty)
    val scriptFile = touchFileScript
    showScriptUnderTest(scriptFile.getName)
    val expectedJar = script2jar(scriptFile)
    sys.props("script.path") = scriptFile.absPath
    val mainArgs: Array[String] = Array(
      "-classpath", TestConfiguration.basicClasspath,
      "-save",
      "-script", scriptFile.toString,
      "-compile-only"
    )

    expectedJar.delete()
    Main.main(mainArgs) // create executable jar
    printf("===> test script jar name [%s]\n", expectedJar.name)
    assert(File.getOnDisk(expectedJar.path).nonEmpty, s"unable to create executable jar [$expectedJar]")

    File.getOnDisk(touchedFileName).foreach(_.delete())
    printf("calling executable jar %s\n", expectedJar)
    callExecutableJar(scriptFile, expectedJar)
    File.getOnDisk(touchedFileName) match
      case Some(f) =>
        printf("success: executable jar created file %s\n", f.path)
        f.delete()
      case None =>
        throw new AssertionError(s"expected to find file ${touchedFileName}" )

///////////////////////////////////
  def touchFileScript = testFiles.find(_.getName == "touchFile.sc").get

  val touchedFileName = "touchedFile.out"

  def script2jar(scriptFile: java.io.File) =
    val f = File.getOnDisk(scriptFile.getPath).get
    f.parent.getOrCreateFile(f.nameWithoutExt, FileExtension.Jar)

  def showScriptUnderTest(name: String): Unit =
    printf("===> test script name [%s]\n", name)

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

  def callExecutableJar(script: java.io.File, jar: File, scriptArgs: Array[String] = Array.empty[String]) = {
    import scala.sys.process.*
    val cmd = Array("java", s"-Dscript.path=${script.getName}", "-jar", jar.path)
      ++ scriptArgs
    Process(cmd).lazyLines_!.foreach { println }
  }
