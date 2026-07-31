package dotty.tools

import java.io.ByteArrayOutputStream
import dotty.tools.nio.*

import org.junit.Assert.assertEquals
import org.junit.Test

class MainGenericCompilerTest:
  @Test def targetScriptCanBeOpened(): Unit =
    val script = File.createTemporaryOnDisk("target-script.scala")
    val initial = CompileSettings(scriptArgs = List("argument"))
    try
      assertEquals(
        initial.copy(targetScript = script.path),
        initial.withTargetScript(script.path)
      )
    finally script.delete()

  @Test def targetScriptMustBeAnOpenableFile(): Unit =
    val directory = FileContainer.createTemporaryOnDisk("target-script")
    val initial = CompileSettings(targetScript = "previous.scala")
    try
      for path <- List(directory.path + FileSystemEntry.separator + "missing.scala", directory.path) do
        val output = new ByteArrayOutputStream
        val settings = Console.withOut(output) {
          initial.withTargetScript(path)
        }
        assertEquals(initial.copy(exitCode = 2), settings)
        assertEquals(s"not found $path${System.lineSeparator()}", output.toString)
    finally directory.deleteRecursively()

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
