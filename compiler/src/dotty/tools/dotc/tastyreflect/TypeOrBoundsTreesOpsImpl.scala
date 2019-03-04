package dotty.tools.dotc.tastyreflect

trait TypeOrBoundsTreesOpsImpl extends scala.tasty.reflect.TypeOrBoundsTreeOps with CoreImpl {
  def typeTreeAsParent(typeTree: TypeTree): TermOrTypeTree = typeTree
}
