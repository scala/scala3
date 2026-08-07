package dotty.tools.scaladoc
package e2e

import java.nio.file.Files
import java.io.File
import org.junit.Test
import org.junit.Assert.*
import dotty.tools.scaladoc.util.IO

/** End-to-end Scaladoc runs that compile sources at test time and assert on diagnostics. */
class EndToEndTests:
  private def run(dirName: String, fileNames: String*)(callback: (File, CompilerContext) => Unit): Unit =
    val root = Files.createTempDirectory("scaladoc-e2e")
    try
      val output = root.resolve("classes")
      compileStage(output, Nil, fileNames.map(copyTestResource(root, dirName, _))*)
      val ctx = testContext
      val docOutput = root.resolve("doc").toFile
      val tasty = collectTastyFiles(output)
      assert(tasty.nonEmpty, s"Expected .tasty files under $output")
      Scaladoc.run(
        testArgs(tasty, docOutput).copy(
          classpath = Seq(output.toString, javaClasspath).mkString(java.io.File.pathSeparator)
        )
      )(using ctx)
      callback(docOutput, ctx)
    finally
      IO.delete(root.toFile)

  private def ensureContains(docOutput: File, packageName: String, fileName: String, texts: String*): Unit = {
    val contents = IO.read(new File(new File(docOutput, packageName), fileName).toPath)
    texts.foreach(text =>
      assertTrue(s"$packageName/$fileName should contain '$text'.\nFull contents:\n$contents", contents.contains(text))
    )
  }

  @Test
  def i26627(): Unit = run("i26627", "lazyFuture.scala") { (_, ctx) =>
    val diagnostics = ctx.reportedDiagnostics
    val linkWarnings = diagnostics.warningMsgs.filter(msg =>
      msg.contains("Couldn't resolve a member for the given link query")
        || msg.contains("Unable to find a link for")
    )
    assertEquals(
      s"Unexpected unresolved link warnings:\n${linkWarnings.mkString("\n")}",
      Nil,
      linkWarnings
    )
    assertNoErrors(diagnostics)
  }

  @Test
  def definesAreUsedInDescription(): Unit = run("defines", "usageInSelf.scala") { (out, _) =>
    // this sentence is set before the `@define`s themselves, but they should still be used
    ensureContains(out, "pkg", "T.html", "It defines t as trait and tt as trait.")
  }

  @Test
  def definesAreInherited(): Unit = run("defines", "inheritance.scala") { (out, _) =>
    ensureContains(out, "pkg", "T.html", "A method on the trait (trait)", "A final method on the trait")
    ensureContains(out, "pkg", "C1.html", "A method on the trait (trait)", "A final method on the trait")
    ensureContains(out, "pkg", "C2.html", "A method on the trait (trait)", "A final method on the trait")
    ensureContains(out, "pkg", "C3.html", "A method on the class (class)", "A final method on the class")
    ensureContains(out, "pkg", "C4.html", "A method on the class (class4)", "A final method on the class4")
    ensureContains(out, "pkg", "C5.html", "A method on the trait (trait)", "A final method on the trait")
  }

  @Test
  def escapes(): Unit = run("defines", "escapes.scala") { (out, _) =>
    ensureContains(out, "pkg", "C.html",
      "Normal: value, Escaped: $c, Recursive: recursive-value, Defined-Escaped: escaped-$c, Alone: $"
    )
  }

  @Test
  def superReference(): Unit = run("defines", "super.scala") { (out, _) =>
    ensureContains(out, "pkg", "Derived.html", "Hello World!", "Derived from Base.")
  }

  @Test
  def braces(): Unit = run("defines", "braces.scala") { (out, _) =>
    ensureContains(out, "pkg", "C.html", "value, value, other, other, more, more, last, last.")
  }

  @Test
  def macros(): Unit = run("defines", "macros.scala") { (out, _) =>
    ensureContains(out, "pkg", "Super.html", "Super.inherited: Super", "Super.implemented: Super", "Super.overridden: Super")
    ensureContains(out, "pkg", "A.html", "List default", "Gets the default with the given dummy")
    ensureContains(out, "pkg", "B.html", "List default", "Gets the default with the given id")
    ensureContains(out, "pkg", "Sub.html",
      "Super.inherited: Sub", "Super.implemented: Sub", "Super.overridden: Sub",
      "List banana", "Gets the banana with the given id"
    )
  }

  @Test
  def selfType(): Unit = run("defines", "selfType.scala") { (out, _) =>
    ensureContains(out, "pkg", "First.html", "m: xxx")
    ensureContains(out, "pkg", "Second.html", "m2: xxx yyy")
    ensureContains(out, "pkg", "C.html", "m: xxx", "m2: xxx overridden")
  }

  @Test
  def fences(): Unit = run("defines", "fences.scala") { (out, _) =>
    ensureContains(out, "pkg", "C.html", "$a", "${b}", "$c", "${d}", "$e")
  }

  // for advanced cases found while generating the doc of the stdlib
  @Test
  def stdlibCasesNoWarnings(): Unit = run("defines", "stdlib.scala") { (_, ctx) =>
    assertTrue(
      ctx.reportedDiagnostics.warningMsgs.mkString("\n"),
      !ctx.reportedDiagnostics.warningMsgs.exists(m => m.contains("undefined in comment") || m.contains("Couldn't resolve"))
    )
  }

  @Test
  def i20028(): Unit = run("i20028", "Enum.scala", "Foo.scala") { (_, ctx) =>
    assertTrue(
      ctx.reportedDiagnostics.warningMsgs.mkString("\n"),
      !ctx.reportedDiagnostics.warningMsgs.exists(m => m.contains("Couldn't resolve"))
    )
  }
