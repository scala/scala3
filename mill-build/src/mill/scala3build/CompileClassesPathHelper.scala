package mill.scala3build

import mill.api.BuildCtx
import mill.scalalib.*

trait CompileClassesPathHelper extends ScalaModule {
  def compileClassesPath0 = compileClassesPath.resolve(BuildCtx.workspaceRoot / "out" /* FIXME */)

  def jarPath =
    resolveRelativeToOut(jar, identity).resolve(BuildCtx.workspaceRoot / "out" /* FIXME */)
}
