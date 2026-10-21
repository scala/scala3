package dotty.tools.scaladoc
package e2e

import java.nio.file.Files
import java.io.File
import org.junit.Test
import org.junit.Assert.*
import dotty.tools.scaladoc.util.IO
import dotty.tools.dotc.reporting.Reporter

/** End-to-end Scaladoc runs that compile sources at test time and assert on diagnostics. */
class EndToEndTests:
  private def run(dirName: String, fileNames: String*)(callback: (File, Reporter) => Unit): Unit =
    val root = Files.createTempDirectory("scaladoc-e2e")
    try
      val output = root.resolve("classes")
      compileStage(output, Nil, fileNames.map(copyTestResource(root, dirName, _))*)
      val docOutput = root.resolve("doc").toFile
      java.nio.file.Files.createDirectories(docOutput.toPath)
      val tasty = collectTastyFiles(output)
      assert(tasty.nonEmpty, s"Expected .tasty files under $output")
      val args = Array(
        "-project", "Test Project Name",
        "-d", docOutput.toString,
        "-cp", Seq(output.toString, javaClasspath).mkString(java.io.File.pathSeparator)
      ) ++ tasty.map(_.toString)
      val reporter = (new dotty.tools.scaladoc.Main).run(args)
      callback(docOutput, reporter)
    finally
      IO.delete(root.toFile)

  private def ensureContains(docOutput: File, packageName: String, fileName: String, texts: String*): Unit = {
    val contents = IO.read(new File(new File(docOutput, packageName), fileName).toPath)
    texts.foreach(text =>
      assertTrue(s"$packageName/$fileName should contain '$text'.\nFull contents:\n$contents", contents.contains(text))
    )
  }

  @Test
  def i26627(): Unit = run("i26627", "lazyFuture.scala") { (_, reporter) =>
    val linkWarnings = reporter.allWarnings.map(_.message).filter(msg =>
      msg.contains("Couldn't resolve a member for the given link query")
        || msg.contains("Unable to find a link for")
    )
    assertEquals(
      s"Unexpected unresolved link warnings:\n${linkWarnings.mkString("\n")}",
      Nil,
      linkWarnings
    )
    assertEquals(0, reporter.errorCount)
  }

  @Test
  def definesAreUsedInDescription(): Unit = run("defines", "usageInSelf.scala") { (out, _) =>
    // this sentence is set before the `@define`s themselves, but they should still be used
    ensureContains(out, "pkg", "T.html", "It defines t as trait and tt as trait.")
  }

  @Test
  def definesAreInherited(): Unit = run("defines", "inheritance.scala") { (out, _) =>
    // See the comment in CommentParsing.tagIndex for why we have extra whitespace
    ensureContains(out, "pkg", "T.html", "A method on the trait (trait )", "A final method on the trait")
    ensureContains(out, "pkg", "C1.html", "A method on the trait (trait )", "A final method on the trait")
    ensureContains(out, "pkg", "C2.html", "A method on the trait (trait )", "A final method on the trait")
    ensureContains(out, "pkg", "C3.html", "A method on the class (class)", "A final method on the class")
    ensureContains(out, "pkg", "C4.html", "A method on the class (class4)", "A final method on the class4")
    ensureContains(out, "pkg", "C5.html", "A method on the trait (trait )", "A final method on the trait")
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

  @Test
  def variableDefinedToItself(): Unit = run("defines", "variableDefinedToItself.scala") { (out, _) =>
    // Nothing, we just don't want this to loop infinitely
  }

  @Test
  def spaces(): Unit = run("defines", "spaces.scala") { (_, reporter) =>
    // This used to crash and thus cause a warning
    assertTrue(reporter.allErrors.mkString("\n"), reporter.allErrors.isEmpty)
    assertTrue(reporter.allWarnings.mkString("\n"), !reporter.allWarnings.exists(_.message.contains("IndexOutOfBounds")))
  }

  @Test
  def markdown(): Unit = run("defines", "markdown.scala") { (out, _) =>
    ensureContains(out, "pkg", "C.html", "before <strong>middle</strong> after")
  }

  // for advanced cases found while generating the doc of the stdlib
  @Test
  def stdlibCasesNoWarnings(): Unit = run("defines", "stdlib.scala") { (_, reporter) =>
    assertTrue(
      reporter.allWarnings.mkString("\n"),
      !reporter.allWarnings.exists(m => m.message.contains("undefined in comment") || m.message.contains("Couldn't resolve"))
    )
  }

  @Test
  def inheritdocSimple(): Unit = run("inheritdoc", "simple.scala") { (out, _) =>
    // Regression test for a case where the @param in the child contained the @return tag
    ensureContains(out, "inheritdoc", "Child.html", "child parameter</p>")
  }

  @Test
  def i20028(): Unit = run("i20028", "Enum.scala", "Foo.scala") { (_, reporter) =>
    assertTrue(
      reporter.allWarnings.mkString("\n"),
      !reporter.allWarnings.exists(m => m.message.contains("Couldn't resolve"))
    )
  }

  @Test
  def throws(): Unit = run("throws", "MyException.scala", "Thrower.scala", "ThrowerDerived.scala") { (_, reporter) =>
    assertTrue(
      reporter.allWarnings.mkString("\n"),
      !reporter.allWarnings.exists(m => m.message.contains("Couldn't resolve"))
    )
  }

  @Test
  def exportMethods(): Unit = run("exports", "exports.scala") { (out, _) =>
    ensureContains(out, "pkg", "Midrange.html", "parameterless method 1", "method with empty parameter list 1", "method with one parameter 1")
    ensureContains(out, "pkg", "Treble.html", "parameterless method 2", "method with empty parameter list 2", "method with one parameter 2")
  }

  @Test
  def warningPosition(): Unit = run("warningPosition", "warningPosition.scala") { (_, reporter) =>
    assertEquals(
      reporter.allWarnings.mkString("\n"),
      2,
      reporter.allWarnings.count(m => m.message.contains("Couldn't resolve"))
    )
  }

  @Test
  def i24438(): Unit = run("24438", "24438.scala") { (_, reporter) =>
    // this used to crash
    assertTrue(reporter.allErrors.mkString("\n"), reporter.allErrors.isEmpty)
  }

  @Test
  def specialLastChars(): Unit = run("special-last-chars", "special-last-chars.scala") { (_, reporter) =>
    assertTrue(
      reporter.allWarnings.mkString("\n"),
      !reporter.allWarnings.exists(m => m.message.contains("Couldn't resolve"))
    )
  }
