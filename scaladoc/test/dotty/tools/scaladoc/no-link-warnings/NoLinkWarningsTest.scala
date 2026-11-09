package dotty.tools.scaladoc
package noLinkWarnings

import org.junit.Test

class NoLinkWarningsTest extends ScaladocTest("noLinkWarnings"):

  override def args = Scaladoc.Args(
    name = "test",
    tastyFiles = tastyFiles(name),
    output = getTempDir().getRoot,
    projectVersion = Some("1.0"),
    noLinkWarnings = true
  )

  @Test
  def runTest(): Unit = afterRendering {
    val diagnostics = summon[DocContext].compilerContext.reportedDiagnostics
    assertNoWarning(diagnostics)
    assertNoErrors(diagnostics)
  }
