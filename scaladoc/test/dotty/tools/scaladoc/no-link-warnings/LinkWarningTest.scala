package dotty.tools.scaladoc
package noLinkWarnings

import org.junit.Assert.assertEquals

class LinkWarningsTest extends ScaladocTest("noLinkWarnings"):

  override def args = Scaladoc.Args(
    name = "test",
    tastyFiles = tastyFiles(name),
    output = getTempDir().getRoot,
    projectVersion = Some("1.0")
  )

  override def runTest = afterRendering {
    val diagnostics = summon[DocContext].compilerContext.reportedDiagnostics
    assertEquals("There should be exactly one warning", 1, diagnostics.warningMsgs.size)
    assertNoErrors(diagnostics)
  }
