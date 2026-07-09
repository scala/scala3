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
      assertTrue(output, output.contains("Added"))
      assertNoMacroFailure(output)

  private def resolveUpickleViaDirective()(using State): Unit =
    run("//> using dep com.lihaoyi::upickle:4.4.3")
    val output = storedOutput()
    assertTrue(output, output.contains("Resolved 1 dependencies"))
    assertNoMacroFailure(output)

  @Test def `i21654 using dep directive resolves like :dep`: Unit =
    initially:
      resolveUpickleViaDirective()

  @Test def `i21654 using dep directive before code resolves and compiles`: Unit =
    initially:
      run("//> using dep com.lihaoyi::upickle:4.4.3\ncase class FooCombined(x: Int) derives upickle.default.ReadWriter")
      val output = storedOutput()
      assertTrue(output, output.contains("Resolved 1 dependencies"))
      assertNoMacroFailure(output)
      assertTrue(output, output.contains("// defined case class FooCombined"))

  @Test def `i21654 multiple dep directives before code resolve together`: Unit =
    initially:
      run("""//> using dep com.lihaoyi::upickle:4.4.3
            |//> using dep com.lihaoyi::os-lib:0.11.3
            |val combined = (upickle.default.write(1), os.pwd.toString)""".stripMargin)
      val output = storedOutput()
      assertTrue(output, output.contains("Resolved 2 dependencies"))
      assertNoMacroFailure(output)
      assertTrue(output, output.contains("val combined:"))

  @Test def `i21654 os-lib directive before code resolves and evaluates`: Unit =
    initially:
      run("//> using dep com.lihaoyi::os-lib:0.11.3\nval p = os.pwd.toString")
      val output = storedOutput()
      assertTrue(output, output.contains("Resolved 1 dependencies"))
      assertNoMacroFailure(output)
      assertTrue(output, output.contains("val p: String ="))

  @Test def `i21654 dep command before code resolves and evaluates`: Unit =
    initially:
      run(":dep com.lihaoyi::os-lib:0.11.3\nval p = os.pwd.toString")
      val output = storedOutput()
      assertTrue(output, output.contains("Resolved 1 dependencies"))
      assertNoMacroFailure(output)
      assertTrue(output, output.contains("val p: String ="))

  @Test def `i21654 repeated dep commands with trailing newline resolve`: Unit =
    initially:
      run(":dep com.lihaoyi::os-lib:0.11.3\n")
      val first = storedOutput()
      assertTrue(first, first.contains("Resolved 1 dependencies"))
      assertNoMacroFailure(first)
      run(":dep com.lihaoyi::upickle:4.4.3\n")
      val second = storedOutput()
      assertTrue(second, second.contains("Resolved 1 dependencies"))
      assertNoMacroFailure(second)
      assertFalse(second, second.contains("Illegal start of statement"))

  @Test def `i25291 derives ReadWriter after :dep`: Unit =
    initially:
      resolveUpickle()
      run("case class FooDerives(x: Int) derives upickle.default.ReadWriter")
      val output = storedOutput()
      assertNoMacroFailure(output)
      assertTrue(output, output.contains("// defined case class FooDerives"))

  @Test def `i25291 macroRW after :dep`: Unit =
    initially:
      resolveUpickle()
      run("case class FooMacroRw(x: Int); object FooMacroRw { given upickle.default.ReadWriter[FooMacroRw] = upickle.default.macroRW }")
      val second = storedOutput()
      assertNoMacroFailure(second)
      assertTrue(second, second.contains("// defined case class FooMacroRw"))

  @Test def `i25291 derives ReadWriter after :jar`: Unit =
    initially:
      addUpickleViaJar()
      run("case class FooJar(x: Int) derives upickle.default.ReadWriter")
      val output = storedOutput()
      assertNoMacroFailure(output)
      assertTrue(output, output.contains("// defined case class FooJar"))

  @Test def `i25291 readwriter summon after :dep`: Unit =
    initially:
      resolveUpickle()
      run("case class FooSummon(x: Int) derives upickle.default.ReadWriter; summon[upickle.default.ReadWriter[FooSummon]]")
      val second = storedOutput()
      assertNoMacroFailure(second)
      assertTrue(second, second.contains("// defined case class FooSummon"))

  @Test def `product toString from dependency after :dep`: Unit =
    initially:
      run(":dep com.lihaoyi::ujson:4.4.3")
      val depOutput = storedOutput()
      assertTrue(depOutput, depOutput.contains("Resolved a dependency"))
      assertNoMacroFailure(depOutput)

      run("ujson.Num(3.5)")
      val output = storedOutput()
      assertNoMacroFailure(output)
      assertTrue(output, output.contains("val res0: ujson.Num = 3.5"))
      assertFalse(output, output.contains("Num(3.5)"))
