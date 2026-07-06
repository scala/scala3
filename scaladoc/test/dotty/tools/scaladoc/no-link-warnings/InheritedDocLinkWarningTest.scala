package dotty.tools.scaladoc
package noLinkWarnings

import org.junit.Assert.assertEquals

/** Inherited doc links must resolve at their definition site, not in the inheriting class's scope.
 *  @see https://github.com/scala/scala3/issues/20028
 */
class InheritedDocLinkWarningTest extends ScaladocTest("inheritedDocLinkWarning"):

  override def args = Scaladoc.Args(
    name = "test",
    tastyFiles = tastyFiles(name),
    output = getTempDir().getRoot,
    projectVersion = Some("1.0")
  )

  override def runTest = afterRendering {
    val diagnostics = summon[DocContext].compilerContext.reportedDiagnostics
    val linkWarnings = diagnostics.warningMsgs.filter(_.contains("link query"))
    assertEquals(
      "The inherited `[[Parent]]` link should currently produce exactly one spurious warning (see #20028)",
      List("Couldn't resolve a member for the given link query: Parent"),
      linkWarnings
    )
    assertNoErrors(diagnostics)
  }
