package dotty.tools
package repl

import org.junit.Assert.{assertFalse, assertTrue}
import org.junit.Test

class ReplDependencyMacroTests extends ReplTest:

  private def assertNoMacroFailure(output: String): Unit =
    assertFalse(output, output.contains("Failed to evaluate macro"))
    assertFalse(output, output.contains("ClassNotFoundException"))
    assertFalse(output, output.contains("Cannot reduce `inline if`"))
    assertFalse(output, output.contains("-- Error:"))

  private def resolveUpickle()(using State): Unit =
    run(":dep com.lihaoyi::upickle:4.4.3")
    val output = storedOutput()
    println(s"[debug][i25291] :dep output:\n$output")
    assertTrue(output, output.contains("Resolved 1 dependencies"))
    assertNoMacroFailure(output)

  private def addUpickleViaJar()(using State): Unit =
    val deps = List(("com.lihaoyi", "upickle_3", "4.4.3"))
    val files = DependencyResolver.resolveDependencies(deps) match
      case Right(value) => value
      case Left(error) =>
        assertTrue(s"Failed to resolve dependency for :jar test: $error", false)
        Nil

    val filesToAdd =
      files.filterNot: file =>
        val name = file.getName
        name.startsWith("scala3-library_3-")
          || name.startsWith("scala-library-")
          || name.startsWith("scala3-interfaces-")
          || name.startsWith("tasty-core_3-")

    assertTrue("No JAR files available to add via :jar", filesToAdd.nonEmpty)

    filesToAdd.foreach: file =>
      run(s":jar ${file.getAbsolutePath}")
      val output = storedOutput()
      println(s"[debug][i25291] :jar output (${file.getName}):\n$output")
      assertTrue(output, output.contains("Added"))
      assertNoMacroFailure(output)

  @Test def `i25291 derives ReadWriter after :dep`: Unit =
    initially:
      resolveUpickle()
      run("case class FooDerives(x: Int) derives upickle.default.ReadWriter")
      val output = storedOutput()
      println(s"[debug][i25291] derives output:\n$output")
      assertNoMacroFailure(output)
      assertTrue(output, output.contains("// defined case class FooDerives"))

  @Test def `i25291 macroRW after :dep`: Unit =
    initially:
      resolveUpickle()
      run("case class FooMacroRw(x: Int); object FooMacroRw { given upickle.default.ReadWriter[FooMacroRw] = upickle.default.macroRW }")
      val second = storedOutput()
      println(s"[debug][i25291] macroRW output:\n$second")
      assertNoMacroFailure(second)
      assertTrue(second, second.contains("// defined case class FooMacroRw"))

  @Test def `i25291 derives ReadWriter after :jar`: Unit =
    initially:
      addUpickleViaJar()
      run("case class FooJar(x: Int) derives upickle.default.ReadWriter")
      val output = storedOutput()
      println(s"[debug][i25291] :jar derives output:\n$output")
      assertNoMacroFailure(output)
      assertTrue(output, output.contains("// defined case class FooJar"))

  @Test def `i25291 readwriter summon after :dep`: Unit =
    initially:
      resolveUpickle()
      run("case class FooSummon(x: Int) derives upickle.default.ReadWriter; summon[upickle.default.ReadWriter[FooSummon]]")
      val second = storedOutput()
      println(s"[debug][i25291] summon output:\n$second")
      assertNoMacroFailure(second)
      assertTrue(second, second.contains("// defined case class FooSummon"))
