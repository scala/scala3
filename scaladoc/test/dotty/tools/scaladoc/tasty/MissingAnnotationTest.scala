package dotty.tools.scaladoc
package tasty

import java.nio.file.Files
import org.junit.Test
import dotty.tools.scaladoc.util.IO

class MissingAnnotationTest:

  @Test
  def ignoresUndocumentedAnnotationsMissingFromDocClasspath =
    val root = Files.createTempDirectory("scaladoc-missing-annotation")
    try
      val annotationOutput = root.resolve("annotation")
      compileStage(annotationOutput, Nil, copyTestResource(root, "missing-annotation", "annotation.scala"))

      val fooOutput = root.resolve("foo")
      compileStage(fooOutput, Seq(annotationOutput), copyTestResource(root, "missing-annotation", "foo.scala"))

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
