package dotty.tools.dotc.transform

import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Types.{Type, TypeRef}
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo}

/**
  * Eliminates syntactic references to Java packages, so that there's no chance
  * they accidentally end up in the backend.
  */
class ElimJavaPackages extends MiniPhaseTransform {

  override def phaseName: String = "elimJavaPackages"

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (isJavaPackage(tree)) {
      assert(tree.tpe.isInstanceOf[TypeRef], s"Expected tree with type TypeRef, but got ${tree.tpe.show}")
      Ident(tree.tpe.asInstanceOf[TypeRef])
    } else {
      tree
    }
  }

  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    tree match {
      case tree: Select =>
        assert(!isJavaPackage(tree), s"Unexpected reference to Java package in ${tree.show}")
      case _ => ()
    }
  }

  /**
    * Is the given tree a syntactic reference to a Java package?
    */
  private def isJavaPackage(tree: Select)(implicit ctx: Context): Boolean = {
    tree.tpe match {
      case TypeRef(prefix, _) =>
        val flags = prefix.termSymbol.flags
        // Testing for each flag separately is more efficient than using FlagConjunction.
        flags.is(Package) && flags.is(JavaDefined)
      case _ => false
    }
  }
}
