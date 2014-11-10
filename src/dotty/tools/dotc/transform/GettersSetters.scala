package dotty.tools.dotc
package transform

import core._
import DenotTransformers.SymTransformer
import Phases.Phase
import Contexts.Context
import SymDenotations.SymDenotation
import Types._
import Symbols._
import SymUtils._
import Constants._
import ast.Trees._
import TreeTransforms._
import NameOps._
import Flags._
import Decorators._

/** Performs the following rewritings on fields of classes, where `x_L` is the "local name" of `x`:
 *
 *    val x: T = e
 *      -->  private val x_L: T = e
 *           <stable> def x: T = x_L
 *
 *    var x: T = e
 *    def x_=(y: T) = ()
 *      -->  private var x_L: T = e
 *           def x: T = x_L
 *           def x_=(x: T): Unit = x_L = x                            (if in class or trait)
 *
 *    lazy val x: T = e
 *      --> def x: T = e
 *
 *    val x: T
 *      -->  <stable> def x: T
 *
 *    var x: T
 *      -->  def x: T
 *
 *  Omitted from the rewritings are
 *
 *   - private[this] fields in non-trait classes
 *   - fields generated for static modules (TODO: needed?)
 *   - parameters, static fields, and fields coming from Java
 *
 *  Furthermore, assignements to mutable vars are replaced by setter calls
 *
 *     p.x = e
 *      -->  p.x_=(e)
 */
 class GettersSetters extends MiniPhaseTransform with SymTransformer { thisTransform =>
  import ast.tpd._

  override def phaseName = "gettersSetters"

  override def treeTransformPhase = thisTransform.next

  override def transformSym(d: SymDenotation)(implicit ctx: Context): SymDenotation = {
    def noGetterNeeded =
      d.is(NoGetterNeeded) ||
      d.initial.asInstanceOf[SymDenotation].is(PrivateLocal) && !d.owner.is(Trait) ||
      d.is(Module) && d.isStatic ||
      d.isSelfSym
    if (d.isTerm && d.owner.isClass && d.info.isValueType && !noGetterNeeded) {
      val maybeStable = if (d.isStable) Stable else EmptyFlags
      //if (d.name.toString == "_") println(i"make accessor $d in ${d.owner} ${d.symbol.id}")
      d.copySymDenotation(
        initFlags = d.flags | maybeStable | AccessorCreationFlags,
        info = ExprType(d.info))
    }
    else d
  }
  private val NoGetterNeeded = Method | Param | JavaDefined | JavaStatic
  private val NoFieldNeeded  = Lazy | Deferred | ParamAccessor

  override def transformValDef(tree: ValDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (tree.symbol is Method) {
      val getter = tree.symbol.asTerm
      assert(getter is Accessor)
      if (getter is NoFieldNeeded)
        DefDef(getter, tree.rhs)
      else {
        val inTrait = getter.owner.is(Trait)
        val field = ctx.newSymbol(
          owner = ctx.owner,
          name = getter.name.fieldName,
          flags = Private | (getter.flags & Mutable),
          info = getter.info.resultType).enteredAfter(thisTransform)
        assert(tree.rhs.tpe.exists, tree.show)
        val fieldDef =
          cpy.ValDef(tree)(
            mods = tree.mods & EmptyFlags | field.flags,
            name = field.name,
            rhs = tree.rhs.changeOwner(getter, field).ensureConforms(field.info.widen)
          ).withType(field.valRef)
        val rhs = ref(field)
        assert(rhs.hasType)
        val getterDef = DefDef(getter, rhs.ensureConforms(getter.info.widen))
        Thicket(fieldDef, getterDef)
      }
    }
    else tree
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree =
    if (tree.symbol.isSetter && !tree.symbol.is(Deferred | ParamAccessor)) {
      val Literal(Constant(())) = tree.rhs
      assert(tree.symbol.field.exists, i"no field for ${tree.symbol.showLocated}")
      val initializer = Assign(ref(tree.symbol.field), ref(tree.vparamss.head.head.symbol))
      assert(initializer.hasType)
      cpy.DefDef(tree)(rhs = initializer)
    }
    else tree

  override def transformAssign(tree: Assign)(implicit ctx: Context, info: TransformerInfo): Tree =
    if (tree.lhs.symbol is Method) tree.lhs.becomes(tree.rhs)
    else tree
}