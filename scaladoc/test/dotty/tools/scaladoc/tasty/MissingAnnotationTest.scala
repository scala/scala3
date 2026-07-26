package dotty.tools.scaladoc
package tasty

import java.nio.file.{Files, Path, Paths}
import org.junit.{Assert, Test}
import dotty.tools.scaladoc.util.IO

class MissingAnnotationTest:

  private val javaClasspath = System.getProperty("java.class.path")

  private def source(root: Path, name: String): Path =
    val resource = getClass.getResource(s"/missing-annotation/$name.txt")
    val source = root.resolve("sources").resolve(name)
    Files.createDirectories(source.getParent)
    Files.copy(Paths.get(resource.toURI), source)
    source

  private def compileStage(output: Path, classpath: Seq[Path], sources: Path*): Unit =
    Files.createDirectories(output)
    val compilerClasspath =
      (classpath.map(_.toString) :+ javaClasspath).mkString(java.io.File.pathSeparator)
    val reporter = new TestReporter
    val result = dotty.tools.dotc.Main.process(
      Array("-classpath", compilerClasspath, "-d", output.toString) ++ sources.map(_.toString),
      reporter
    )
    Assert.assertFalse(
      s"Compilation failed:\n${reporter.errors.result().map(_.msg.message).mkString("\n")}",
      result.hasErrors
    )

  @Test
  def ignoresUndocumentedAnnotationsMissingFromDocClasspath =
    val root = Files.createTempDirectory("scaladoc-missing-annotation")
    try
      val annotationOutput = root.resolve("annotation")
      compileStage(annotationOutput, Nil, source(root, "annotation.scala"))

      val fooOutput = root.resolve("foo")
      compileStage(fooOutput, Seq(annotationOutput), source(root, "foo.scala"))

      // Run Scaladoc without annotation tasty
      val ctx = testContext
      val docOutput = root.resolve("doc").toFile
      Scaladoc.run(
        testArgs(Seq(fooOutput.resolve("foo/Foo.tasty").toFile), docOutput).copy(
          classpath = Seq(fooOutput.toString, javaClasspath)
            .mkString(java.io.File.pathSeparator)
        )
      )(using ctx)

      val diagnostics = ctx.reportedDiagnostics
      assertNoErrors(diagnostics)
      assertNoWarning(diagnostics)
    finally IO.delete(root.toFile)
