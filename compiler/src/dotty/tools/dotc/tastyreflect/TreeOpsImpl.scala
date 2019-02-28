package dotty.tools.dotc.tastyreflect

trait TreeOpsImpl extends scala.tasty.reflect.TreeOps with RootPositionImpl with Helpers {

  def termAsTermOrTypeTree(term: Term): TermOrTypeTree = term

}
