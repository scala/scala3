package dotty.tools.dotc.tastyreflect

trait TreeOpsImpl extends scala.tasty.reflect.TreeOps with CoreImpl {
  def termAsTermOrTypeTree(term: Term): TermOrTypeTree = term
}
