package dotty.tools
package dotc
package transform

import TreeTransforms.{ MiniPhaseTransform, TransformerInfo }
import core._
import Contexts.Context, Types._, Decorators._, Symbols._, DenotTransformers._
import Denotations._, SymDenotations._, Scopes._, StdNames._, NameOps._, Names._
import ast.tpd

class SpecializeFunction1 extends MiniPhaseTransform with DenotTransformer {
  import ast.tpd._

  val phaseName = "specializeFunction1"

  /** Transforms all classes extending `Function1[-T1, +R]` so that
    * they instead extend the specialized version `JFunction$mp...`
    */
  def transform(ref: SingleDenotation)(implicit ctx: Context) = ref match {
    case ShouldTransformDenot(cref, t1, r, func1) =>
      transformDenot(cref, t1, r, func1)
    case _ => ref
  }

  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo) =
    tree match {
      case ShouldTransformTree(func1, t1, r) => transformTree(tree, func1, t1, r)
      case _ => tree
    }

  private[this] val functionName = "JFunction1".toTermName
  private[this] val functionPkg  = "scala.compat.java8.".toTermName
  private[this] var Function1: Symbol = _
  private[this] var argTypes: Set[Symbol] = _
  private[this] var returnTypes: Set[Symbol] = _
  private[this] var blacklisted: Set[Symbol] = _

  /** Do setup of `argTypes` and `returnTypes` */
  override def prepareForUnit(tree: Tree)(implicit ctx: Context) = {
    argTypes = Set(defn.DoubleClass,
                   defn.FloatClass,
                   defn.IntClass,
                   defn.LongClass,
                   defn.UnitClass,
                   defn.BooleanClass)

    returnTypes = Set(defn.DoubleClass,
                      defn.FloatClass,
                      defn.IntClass,
                      defn.LongClass)

    Function1 = ctx.requiredClass("scala.Function1")

    blacklisted = Set(
      "scala.compat.java8.JFunction1",
      "scala.runtime.AbstractFunction1",
      "scala.PartialFunction",
      "scala.runtime.AbstractPartialFunction"
    ).map(ctx.requiredClass(_))

    this
  }

  private object ShouldTransformDenot {
    def unapply(cref: ClassDenotation)(implicit ctx: Context): Option[(ClassDenotation, Type, Type, Type)] = {
      def collectFunc1(xs: List[Type])(implicit ctx: Context): Option[(Type, Type, Type)] =
        xs.collect {
          case func1 @ RefinedType(RefinedType(parent, _, t1), _, r)
            if func1.isRef(Function1) => (t1, r, func1)
        }.headOption

      collectFunc1(cref.info.parentsWithArgs).flatMap { case (t1, r, func1) =>
        if (
          !argTypes.contains(t1.typeSymbol) ||
          !returnTypes.contains(r.typeSymbol) ||
          blacklisted.exists(sym => cref.symbol.derivesFrom(sym))
        ) None
        else Some((cref, t1, r, func1))
      }
    }
  }

  private object ShouldTransformTree {
    def unapply(tree: Template)(implicit ctx: Context): Option[(Tree, Type, Type)] =
      tree.parents
      .map { t => (t.tpe, t) }
      .collect {
        case (tp @ RefinedType(RefinedType(parent, _, t1), _, r), func1)
        if tp.isRef(Function1) &&
           argTypes.contains(t1.typeSymbol) &&
           returnTypes.contains(r.typeSymbol) => (func1, t1, r)
      }
      .headOption
  }

  private def specializedName(name: Name, t1: Type, r: Type)(implicit ctx: Context): Name =
    name.specializedFor(List(t1, r), List(t1, r).map(_.typeSymbol.name))

  def transformDenot(cref: ClassDenotation, t1: Type, r: Type, func1: Type)(implicit ctx: Context): SingleDenotation = {
    val specializedFunction: TypeRef =
      ctx.requiredClassRef(functionPkg ++ specializedName(functionName, t1, r))

    def replaceFunction1(in: List[TypeRef]): List[TypeRef] =
      in.foldRight(List.empty[TypeRef]) { (tp, acc) =>
        val newTp =
          if (tp.isRef(Function1)) specializedFunction
          else tp
        newTp :: acc
      }

    def specializeApply(scope: Scope): Scope = {
      def specializedApply: Symbol = {
        val specializedMethodName = specializedName(nme.apply, t1, r)
        ctx.newSymbol(
          cref.symbol,
          specializedMethodName,
          Flags.Override | Flags.Method,
          specializedFunction.info.decls.lookup(specializedMethodName).info
        )
      }

      val alteredScope = scope.cloneScope
      alteredScope.enter(specializedApply)
      alteredScope
    }

    val ClassInfo(prefix, cls, parents, decls, info) = cref.classInfo
    val newInfo = ClassInfo(prefix, cls, replaceFunction1(in = parents), specializeApply(decls), info)
    cref.copySymDenotation(info = newInfo)
  }

  private def transformTree(tmpl: Template, func1: Tree, t1: Type, r: Type)(implicit ctx: Context) = {
    val specializedFunc1 =
      TypeTree(ctx.requiredClassRef(functionPkg ++ specializedName(functionName, t1, r)))

    val parents = tmpl.parents.foldRight(List.empty[Tree]) { (t, trees) =>
      (if (func1 eq t) specializedFunc1 else t) :: trees
    }

    val body = tmpl.body.foldRight(List.empty[Tree]) {
      case (tree: DefDef, acc) if tree.name == nme.apply => {
        val specializedMethodName = specializedName(nme.apply, t1, r)
        val specializedApply = ctx.owner.info.decls.lookup(specializedMethodName).asTerm

        val forwardingBody =
          tpd.ref(specializedApply)
          .appliedToArgs(tree.vparamss.head.map(vparam => ref(vparam.symbol)))

        val applyWithForwarding = cpy.DefDef(tree)(rhs = forwardingBody)

        val specializedApplyDefDef = polyDefDef(specializedApply, trefs => vrefss => {
          tree.rhs
            .changeOwner(tree.symbol, specializedApply)
            .subst(tree.vparamss.flatten.map(_.symbol), vrefss.flatten.map(_.symbol))
        })

        applyWithForwarding :: specializedApplyDefDef :: acc
      }
      case (tree, acc) => tree :: acc
    }

    cpy.Template(tmpl)(parents = parents, body = body)
  }
}
