package dotty.tools
package dotc
package transform

import ast.tpd
import core.*, Contexts.*, Decorators.*, Symbols.*, Flags.*, StdNames.*
import reporting.trace
import util.Property
import MegaPhase.*

/** This phase rewrites calls to `Array.apply` to a direct instantiation of the array in the bytecode.
 *
 *  Transforms `scala.Array.apply([....])` and `scala.Array.apply(..., [....])` into `[...]`
 */
class ArrayApply extends MiniPhase {
  import tpd.*

  override def phaseName: String = ArrayApply.name

  override def description: String = ArrayApply.description

  private val TransformListApplyBudgetKey = new Property.Key[Int]
  private def transformListApplyBudget(using Context) =
    ctx.property(TransformListApplyBudgetKey).getOrElse(8) // default is 8, as originally implemented in nsc

  // A single-node `Vector1` holds up to `WIDTH` = 32 elements; longer `Seq(...)`/`Vector(...)`
  // literals are left for the factory apply to build a multi-level vector. Unlike the `List`
  // cons rewrite (whose budget bounds cons-chain code size), the `Vector` rewrite just wraps the
  // array that the varargs would build anyway, so it is bounded only by `Vector1`'s capacity.
  private inline val MaxVector1Length = 32

  override def prepareForApply(tree: Apply)(using Context): Context = tree match
    case SeqApplyArgs(elems) if isListApply(tree) =>
      ctx.fresh.setProperty(TransformListApplyBudgetKey, transformListApplyBudget - elems.length)
    case _ => ctx

  override def transformApply(tree: Apply)(using Context): Tree =
    if isArrayModuleApply(tree.symbol) then
      tree.args match
        case StripAscription(Apply(wrapRefArrayMeth, (seqLit: JavaSeqLiteral) +: Vector())) +: ct +: Vector()
            if defn.WrapArrayMethods().contains(wrapRefArrayMeth.symbol) && elideClassTag(ct) =>
          seqLit

        case elem0 +: StripAscription(Apply(wrapRefArrayMeth, (seqLit: JavaSeqLiteral) +: Vector())) +: Vector()
            if defn.WrapArrayMethods().contains(wrapRefArrayMeth.symbol) =>
          JavaSeqLiteral(elem0 +: seqLit.elems, seqLit.elemtpt)

        case _ =>
          tree

    else tree match
      case SeqApplyArgs(elems) =>
        def vectorApply =
          // The literal's length is statically known, so emit the exact `Vector` node directly
          // with no runtime length check.
          if elems.isEmpty then
            ref(defn.Vector_empty).ensureApplied.cast(tree.tpe) // Seq()/Vector() ~> Vector.empty
          else if elems.length <= MaxVector1Length then
            // Seq(a, b, c)/Vector(a, b, c) ~> new Vector1([a, b, c]): builds the backing array
            // directly and wraps it, avoiding the intermediate `ArraySeq` wrapper and factory dispatch.
            val arr = JavaSeqLiteral(elems.map(_.ensureConforms(defn.ObjectType)), TypeTree(defn.ObjectType))
            New(defn.Vector1Class.typeRef, Vector(arr)).cast(tree.tpe)
          else
            tree // more than WIDTH elements: leave the factory apply to build a multi-level vector
        if !isListApply(tree) then vectorApply
        else if transformListApplyBudget > 0 || elems.isEmpty then
          // `List(a, b, c)` ~> new ::(a, new ::(b, new ::(c, Nil)))
          val consed = elems.foldRight(ref(defn.NilModule)): (elem, acc) =>
            New(defn.ConsType, Vector(elem.ensureConforms(defn.ObjectType), acc))
          consed.cast(tree.tpe)
        else
          tree
      case _ => tree

  private def isArrayModuleApply(sym: Symbol)(using Context): Boolean =
    sym.name == nme.apply
    && (sym.owner == defn.ArrayModuleClass || (sym.owner == defn.IArrayModuleClass && !sym.is(Extension)))

  private def isListApply(tree: Tree)(using Context): Boolean =
    (tree.symbol == defn.ListModule_apply || tree.symbol.name == nme.apply) && appliedCore(tree).match
      case Select(qual, _) =>
        val sym = qual.symbol
        sym == defn.ListModule
        || sym == defn.ListModuleAlias
      case _ => false

  private def isSeqApply(tree: Tree)(using Context): Boolean =
    isListApply(tree) || (tree.symbol == defn.SeqModule_apply || tree.symbol.name == nme.apply) && appliedCore(tree).match
      case Select(qual, _) =>
        val sym = qual.symbol
        sym == defn.SeqModule
        || sym == defn.SeqModuleAlias
        || sym == defn.CollectionSeqType.symbol.companionModule
        || sym == defn.VectorModule
        || sym == defn.VectorModuleAlias
      case _ => false

  private object SeqApplyArgs:
    def unapply(tree: Apply)(using Context): Option[Vector[Tree]] =
      if isSeqApply(tree) then
        tree.args match
          // <List, Seq, or Vector>(a, b, c) after vararg conversion
          case StripAscription(Apply(wrapArrayMeth, Vector(StripAscription(rest: JavaSeqLiteral)))) +: Vector()
              if rest.elems.isEmpty || isVarargsArrayWrapper(wrapArrayMeth.symbol) =>
            Some(rest.elems)
          case _ => None
      else None

  private def isVarargsArrayWrapper(sym: Symbol)(using Context): Boolean =
    defn.WrapArrayMethods().contains(sym)
    || sym == defn.getWrapVarargsArrayModule.requiredMethod(nme.genericWrapArray)

  /** Only optimize when classtag if it is one of
   *  - `ClassTag.apply(classOf[XYZ])`
   *  - `ClassTag.apply(java.lang.XYZ.Type)` for boxed primitives `XYZ``
   *  - `ClassTag.XYZ` for primitive types
   */
  private def elideClassTag(ct: Tree)(using Context): Boolean = ct match {
    case Apply(_, rc +: Vector()) if ct.symbol == defn.ClassTagModule_apply =>
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

  object StripAscription {
    def unapply(tree: Tree)(using Context): Some[Tree] = tree match {
      case Typed(expr, _) => unapply(expr)
      case _ => Some(tree)
    }
  }
}

object ArrayApply:
  val name: String = "arrayApply"
  val description: String = "optimize `scala.Array.apply`"
