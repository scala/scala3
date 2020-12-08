package dotty
package tools
package scripting

import java.io.File

import org.junit.Test

import vulpix.TestConfiguration


/** Runs all tests contained in `compiler/test-resources/repl/` */
class ScriptingTests:
  extension (str: String) def dropExtension =
    str.reverse.dropWhile(_ != '.').drop(1).reverse

  @Test def scriptingTests =
    val testFiles = scripts("/scripting")

    val argss: Map[String, Array[String]] = (
      for
        argFile <- testFiles
        if argFile.getName.endsWith(".args")
        name = argFile.getName.dropExtension
        scriptArgs = readLines(argFile).toArray
      yield name -> scriptArgs).toMap

    for
      scriptFile <- testFiles
      if scriptFile.getName.endsWith(".scala")
      name = scriptFile.getName.dropExtension
      scriptArgs = argss.getOrElse(name, Array.empty[String])
    do
      ScriptingDriver(
        compilerArgs = Array(
            "-classpath", TestConfiguration.basicClasspath,
            "-color:never",
            "-Yerased-terms",
          ),
        scriptFile = scriptFile,
        scriptArgs = scriptArgs
      ).compileAndRun()
