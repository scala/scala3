package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts._
import Symbols._
import Flags._
import StdNames._
import dotty.tools.dotc.ast.tpd



/** This phase rewrites calls to `Array.apply` to a direct instantiation of the array in the bytecode.
 *
 *  Transforms `scala.Array.apply([....])` and `scala.Array.apply(..., [....])` into `[...]`
 */
class ArrayApply extends MiniPhase:
  import tpd._

  override def phaseName: String = ArrayApply.name

  override def description: String = ArrayApply.description

  override def transformApply(tree: tpd.Apply)(using Context): tpd.Tree =
    if isArrayModuleApply(tree.symbol) then
      tree.args match {
        case AppliedLiterals(seqLit) :: ct :: Nil if elideClassTag(ct) =>
          seqLit

        case InlinedSplice(inlined, seqLit) :: ct :: Nil if elideClassTag(ct) =>
          tpd.cpy.Inlined(inlined)(inlined.call, inlined.bindings, seqLit)

        case elem0 :: AppliedLiterals(seqLit) :: Nil =>
          tpd.JavaSeqLiteral(elem0 :: seqLit.elems, seqLit.elemtpt)

        case elem0 :: InlinedSplice(inlined, seqLit) :: Nil =>
          tpd.cpy.Inlined(inlined)(inlined.call, inlined.bindings, tpd.JavaSeqLiteral(elem0 :: seqLit.elems, seqLit.elemtpt))

        case _ =>
          tree
      }

    else tree

  private def isArrayModuleApply(sym: Symbol)(using Context): Boolean =
    sym.name == nme.apply
    && (sym.owner == defn.ArrayModuleClass || (sym.owner == defn.IArrayModuleClass && !sym.is(Extension)))

  /** Only optimize when classtag if it is one of
   *  - `ClassTag.apply(classOf[XYZ])`
   *  - `ClassTag.apply(java.lang.XYZ.Type)` for boxed primitives `XYZ``
   *  - `ClassTag.XYZ` for primitive types
   */
  private def elideClassTag(ct: Tree)(using Context): Boolean = ct match {
    case Inlined(_, _, expansion) => elideClassTag(expansion)
    case Apply(_, rc :: Nil) if ct.symbol == defn.ClassTagModule_apply =>
      rc match {
        case _: Literal => true // ClassTag.apply(classOf[XYZ])
        case rc: RefTree if rc.name == nme.TYPE_ =>
          // ClassTag.apply(java.lang.XYZ.Type)
          defn.ScalaBoxedClasses().contains(rc.symbol.maybeOwner.companionClass)
        case _ => false
      }
    case Apply(ctm: RefTree, _) if ctm.symbol.maybeOwner.companionModule == defn.ClassTagModule =>
      // ClassTag.XYZ
      nme.ScalaValueNames.contains(ctm.name)
    case _ => false
  }

  // Match a sequence of literal arguments passed to an Array constructor
  private object AppliedLiterals:

    def unapply(tree: Tree)(using Context): Option[tpd.JavaSeqLiteral] = tree match
      case Apply(wrapRefArrayMeth, (seqLit: tpd.JavaSeqLiteral) :: Nil)
          if defn.WrapArrayMethods().contains(wrapRefArrayMeth.symbol) =>
        Some(seqLit)
      case _ => None

  end AppliedLiterals

  // Match an inlined sequence splice
  private object InlinedSplice:
    def unapply(tree: Tree)(using Context): Option[(Inlined, tpd.JavaSeqLiteral)] = tree match
      case inlined @ Inlined(_, _, Typed(AppliedLiterals(seqLit), _)) =>
        Some((inlined, seqLit))
      case _ => None

  end InlinedSplice

end ArrayApply

object ArrayApply:
  val name: String = "arrayApply"
  val description: String = "optimize `scala.Array.apply`"
