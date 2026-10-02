package dotty.tools.scaladoc
package tasty

class I18832Test extends ScaladocTest("i18832"):

  // The crash only happens when `Foo.tasty` is read before `i18832$package.tasty`
  override def args = super.args.copy(tastyFiles = tastyFiles(name).sortBy(_.getName))

  override def runTest = afterRendering {
    val diagnostics = summon[DocContext].compilerContext.reportedDiagnostics
    assertNoErrors(diagnostics)
  }
