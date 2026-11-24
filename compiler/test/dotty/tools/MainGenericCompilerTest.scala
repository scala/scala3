package dotty.tools

import org.junit.Assert.assertEquals
import org.junit.Test

class MainGenericCompilerTest:
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
