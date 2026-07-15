package dotty.tools.scaladoc
package noLinkWarnings

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
    assertNoWarning(diagnostics)
    assertNoErrors(diagnostics)
  }
