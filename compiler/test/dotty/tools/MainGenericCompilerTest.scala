package dotty.tools

import java.io.ByteArrayOutputStream
import java.nio.file.Files

import org.junit.Assert.assertEquals
import org.junit.Test

class MainGenericCompilerTest:
  @Test def targetScriptCanBeOpened(): Unit =
    val script = Files.createTempFile("target-script", ".scala")
    val initial = CompileSettings(scriptArgs = List("argument"))
    try
      assertEquals(
        initial.copy(targetScript = script.toString),
        initial.withTargetScript(script.toString)
      )
    finally Files.delete(script)

  @Test def targetScriptMustBeAnOpenableFile(): Unit =
    val directory = Files.createTempDirectory("target-script")
    val initial = CompileSettings(targetScript = "previous.scala")
    try
      for path <- List(directory.resolve("missing.scala"), directory) do
        val output = new ByteArrayOutputStream
        val settings = Console.withOut(output) {
          initial.withTargetScript(path.toString)
        }
        assertEquals(initial.copy(exitCode = 2), settings)
        assertEquals(s"not found $path${System.lineSeparator()}", output.toString)
    finally Files.delete(directory)

  @Test def javaPropertyValuesAreParsedInFull(): Unit =
    val cases: List[(String, (String, String))] = List(
      "-Dkey=" -> ("key" -> ""),
      "-Dkey=x" -> ("key" -> "x"),
      "-Dkey=World3" -> ("key" -> "World3"),
      "-Dkey=a=b" -> ("key" -> "a=b"),
    )

    cases.foreach { (arg, expected) =>
      val settings = MainGenericCompiler.process(List(arg), CompileSettings())
      assertEquals(List(expected), settings.javaProps)
      assertEquals(Nil, settings.residualArgs)
    }
