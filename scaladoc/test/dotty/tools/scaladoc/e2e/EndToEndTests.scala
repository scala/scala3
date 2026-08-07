package dotty.tools.scaladoc
package e2e

import java.nio.file.Files
import org.junit.Test
import dotty.tools.scaladoc.util.IO

/** End-to-end Scaladoc runs that compile sources at test time and assert on diagnostics. */
class EndToEndTests:

  @Test
  def i26627 =
    val root = Files.createTempDirectory("scaladoc-i26627")
    try
      val output = root.resolve("classes")
      compileStage(output, Nil, copyTestResource(root, "i26627", "lazyFuture.scala"))

      val ctx = testContext
      val docOutput = root.resolve("doc").toFile
      val tasty = collectTastyFiles(output)
      assert(tasty.nonEmpty, s"Expected .tasty files under $output")
      Scaladoc.run(
        testArgs(tasty, docOutput).copy(
          classpath = Seq(output.toString, javaClasspath)
            .mkString(java.io.File.pathSeparator)
        )
      )(using ctx)

      val diagnostics = ctx.reportedDiagnostics
      val linkWarnings = diagnostics.warningMsgs.filter(msg =>
        msg.contains("Couldn't resolve a member for the given link query")
          || msg.contains("Unable to find a link for")
      )
      org.junit.Assert.assertEquals(
        s"Unexpected unresolved link warnings:\n${linkWarnings.mkString("\n")}",
        Nil,
        linkWarnings
      )
      assertNoErrors(diagnostics)
    finally IO.delete(root.toFile)
