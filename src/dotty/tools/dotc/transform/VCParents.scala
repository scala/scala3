package dotty.tools.dotc
package transform

import ast.{Trees, tpd}
import core._, core.Decorators._
import Contexts._, Flags._, Trees._, Types._, StdNames._, Symbols._
import Denotations._, SymDenotations._
import DenotTransformers._, TreeTransforms._, Phases.Phase
import ValueClasses._

/** This phase makes value classes extend VCXPrototype and make their companions extend VCXCompanion.
 *
 *  For a value class class V whose erased underlying type is U, X is "U" if U is a primitive
 *  type and is "Object" otherwise.
 *
 */
class VCParents extends MiniPhaseTransform with DenotTransformer {
  import tpd._

  override def phaseName: String = "vcParents"

  override def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = ref match {
    case moduleClass: ClassDenotation if moduleClass.is(ModuleClass) =>
      moduleClass.linkedClass match {
        case valueClass: ClassSymbol if isDerivedValueClass(valueClass) =>
          val moduleSym = moduleClass.symbol.asClass
          val cinfo = moduleClass.classInfo

          val superType = tpd.ref(defn.vcCompanionOf(valueClass))
            .select(nme.CONSTRUCTOR)
            .appliedToType(valueClass.typeRef)
            .tpe
            .resultType

          moduleClass.copySymDenotation(info =
            cinfo.derivedClassInfo(classParents =
              ctx.normalizeToClassRefs(List(superType), moduleSym, cinfo.decls)))
        case _ =>
          moduleClass
      }
    case valueClass: ClassDenotation if isDerivedValueClass(valueClass) =>
      val cinfo = valueClass.classInfo
      val superType = defn.vcPrototypeOf(valueClass).typeRef

      val (p :: ps) = cinfo.classParents
      assert(p.isRef(defn.AnyValClass))
      val parents = superType :: ps
      valueClass.copySymDenotation(info = cinfo.derivedClassInfo(classParents = parents))
    case _ =>
      ref
  }

  override def transformTemplate(tree: tpd.Template)(implicit ctx: Context, info: TransformerInfo): tpd.Tree =
    ctx.owner.denot match {
      case moduleClass: ClassDenotation if moduleClass.is(ModuleClass) =>
        moduleClass.linkedClass match {
          case valueClass: ClassSymbol if isDerivedValueClass(valueClass) =>
            val superCall = New(defn.vcCompanionOf(valueClass).typeRef)
              .select(nme.CONSTRUCTOR)
              .appliedToType(valueClass.typeRef)
              .appliedToNone

            val (p :: ps) = tree.parents
            // TODO: We shouldn't disallow extending companion objects of value classes
            // with classes other than AnyRef
            assert(p.tpe.isRef(defn.ObjectClass))
            cpy.Template(tree)(parents = superCall :: ps)
          case _ =>
            tree
        }
      case valueClass: ClassDenotation if isDerivedValueClass(valueClass) =>
        val prototype = defn.vcPrototypeOf(valueClass).typeRef
        val underlyingSym = valueClassUnbox(valueClass)
        val superCall = New(prototype, List(ref(underlyingSym)))
        // TODO: manually do parameter forwarding: the prototype has a local field
        // so we don't need a field inside the value class

        val (p :: ps) = tree.parents
        assert(p.tpe.isRef(defn.AnyValClass))
        cpy.Template(tree)(parents = superCall :: ps)
      case _ =>
        tree
    }
}
