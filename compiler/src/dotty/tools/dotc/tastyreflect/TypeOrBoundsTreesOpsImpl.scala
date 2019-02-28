package dotty.tools.dotc.tastyreflect

trait TypeOrBoundsTreesOpsImpl extends scala.tasty.reflect.TypeOrBoundsTreeOps with RootPositionImpl {

  object TypeTree extends TypeTreeModule with TypeTreeCoreModule

  def typeTreeAsParent(typeTree: TypeTree): TermOrTypeTree = typeTree
}
