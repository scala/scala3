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

/** Performs the following rewritings:
 *
 *    val x: T = e
 *      -->  private val x_L: T = e
 *           <stable> def x: T = x_L                                  (if in class)
 *      -->  private notJavaPrivate var x_L: T = e
 *           <stable> def x: T = x_L
 *           private notJavaPrivate def x_=(x: T): Unit = x_L = x     (if in trait)
 *    var x: T = e
 *      -->  private var x_L: T = e
 *           def x: T = x_L
 *           def x_=(x: T): Unit = x_L = x                            (if in class or trait)
 *    lazy val x: T = e
 *      -->  lazy def x = e
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
   /* def noGetterNeeded =
      d.is(Method | Param | JavaDefined) ||
      d.initial.asInstanceOf[SymDenotation].is(PrivateLocal) && !d.owner.is(Trait) ||
      d.is(Module) && d.isStatic ||
      d.isSelfSym
    if (d.isTerm && (d.owner.isClass || d.is(Lazy)) && d.info.isValueType && !noGetterNeeded) {
      val maybeStable = if (d.isStable) Stable else EmptyFlags
      if (d.name.toString == "_") println(i"make accessor $d in ${d.owner} ${d.symbol.id}")
      d.copySymDenotation(
        initFlags = d.flags | maybeStable | AccessorCreationFlags,
        info = ExprType(d.info))
    }
    else */ d
  }
/*
  override def transformValDef(tree: ValDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    if (tree.symbol is Method) {
      val getter = tree.symbol.asTerm
      assert(getter is Accessor)
      if (getter.is(Lazy | Deferred | ParamAccessor)) DefDef(getter, tree.rhs)
      else {
        val inTrait = getter.owner.is(Trait)
        val maybePrivate =
          if (inTrait) Private | NotJavaPrivate
          else if (getter.owner.isClass) Private
          else EmptyFlags
        val maybeMutable =
          if (inTrait || getter.is(Mutable)) Mutable
          else EmptyFlags
        val field = ctx.newSymbol(
          owner = ctx.owner,
          name = getter.name.fieldName,
          flags = maybePrivate | maybeMutable,
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
        if (!getter.is(Mutable) && inTrait) { // add a setter anyway, will be needed for mixin
          val setter = ctx.newSymbol(
              owner = ctx.owner,
              name = getter.name.traitSetterName,
              flags = (getter.flags & AccessFlags) | Accessor | maybePrivate,
              info = MethodType(field.info :: Nil, defn.UnitType)).enteredAfter(thisTransform)
          val setterDef = DefDef(setter.asTerm, vrefss => Assign(ref(field), vrefss.head.head))
          Thicket(fieldDef, getterDef, setterDef)
        }
        else Thicket(fieldDef, getterDef)
      }
    }
    else tree
  }

  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree =
    if (tree.symbol.isSetter && !tree.symbol.is(Deferred | ParamAccessor)) {
      val Literal(Constant(())) = tree.rhs
      val initializer = Assign(ref(tree.symbol.field), ref(tree.vparamss.head.head.symbol))
      assert(initializer.hasType)
      cpy.DefDef(tree)(rhs = initializer)
    }
    else tree

  override def transformAssign(tree: Assign)(implicit ctx: Context, info: TransformerInfo): Tree =
    if (tree.lhs.symbol is Method) tree.lhs.becomes(tree.rhs)
    else tree*/
}