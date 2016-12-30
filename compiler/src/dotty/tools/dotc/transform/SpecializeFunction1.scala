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

  // Setup ---------------------------------------------------------------------
  private[this] val functionName = "JFunction1".toTermName
  private[this] val functionPkg  = "scala.compat.java8.".toTermName
  private[this] var argTypes: Set[Symbol] = _
  private[this] var retTypes: Set[Symbol] = _

  override def prepareForUnit(tree: Tree)(implicit ctx: Context) = {
    retTypes = Set(defn.BooleanClass,
                   defn.DoubleClass,
                   defn.FloatClass,
                   defn.IntClass,
                   defn.LongClass,
                   defn.UnitClass)

    argTypes = Set(defn.DoubleClass,
                   defn.FloatClass,
                   defn.IntClass,
                   defn.LongClass)
    this
  }

  // Transformations -----------------------------------------------------------

  /** Transforms all classes extending `Function1[-T1, +R]` so that
    * they instead extend the specialized version `JFunction$mp...`
    */
  def transform(ref: SingleDenotation)(implicit ctx: Context) = ref match {
    case ShouldTransformDenot(cref, t1, r, func1) => {
      val specializedFunction: Symbol =
        ctx.getClassIfDefined(functionPkg ++ specializedName(functionName, t1, r))

      def replaceFunction1(in: List[TypeRef]): List[TypeRef] =
        in.mapConserve { tp =>
          if (tp.isRef(defn.FunctionClass(1)) && (specializedFunction ne NoSymbol))
            specializedFunction.typeRef
          else tp
        }

      def specializeApply(scope: Scope): Scope =
        if ((specializedFunction ne NoSymbol) && (scope.lookup(nme.apply) ne NoSymbol)) {
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
        else scope

      val ClassInfo(prefix, cls, parents, decls, info) = cref.classInfo
      val newInfo = ClassInfo(prefix, cls, replaceFunction1(in = parents), specializeApply(decls), info)
      cref.copySymDenotation(info = newInfo)
    }
    case _ => ref
  }

  /** Transform the class definition's `Template`:
    *
    * - change the tree to have the correct parent
    * - add the specialized apply method to the template body
    * - forward the old `apply` to the specialized version
    */
  override def transformTemplate(tree: Template)(implicit ctx: Context, info: TransformerInfo) =
    tree match {
      case tmpl @ ShouldTransformTree(func1, t1, r) => {
        val specializedFunc1 =
          TypeTree(ctx.requiredClassRef(functionPkg ++ specializedName(functionName, t1, r)))

        val parents = tmpl.parents.mapConserve { t =>
          if (func1.isDefined && (func1.get eq t)) specializedFunc1 else t
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
      case _ => tree
    }

  /** Dispatch to specialized `apply`s in user code */
  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo) = {
    import ast.Trees._
    tree match {
      case Apply(select @ Select(id, nme.apply), arg :: Nil) =>
        val params = List(arg.tpe, tree.tpe)
        val specializedApply = nme.apply.specializedFor(params, params.map(_.typeSymbol.name), Nil, Nil)
        val hasOverridenSpecializedApply = id.tpe.decls.iterator.exists { sym =>
          sym.is(Flags.Override) && (sym.name eq specializedApply)
        }

        if (hasOverridenSpecializedApply) tpd.Apply(tpd.Select(id, specializedApply), arg :: Nil)
        else tree
      case _ => tree
    }
  }

  private def specializedName(name: Name, t1: Type, r: Type)(implicit ctx: Context): Name =
    name.specializedFor(List(t1, r), List(t1, r).map(_.typeSymbol.name), Nil, Nil)

  // Extractors ----------------------------------------------------------------
  private object ShouldTransformDenot {
    def unapply(cref: ClassDenotation)(implicit ctx: Context): Option[(ClassDenotation, Type, Type, Type)] =
      if (!cref.classParents.exists(_.isRef(defn.FunctionClass(1)))) None
      else getFunc1(cref.typeRef).map { case (t1, r, func1) => (cref, t1, r, func1) }
  }

  private object ShouldTransformTree {
    def unapply(tree: Template)(implicit ctx: Context): Option[(Option[Tree], Type, Type)] =
      tree.parents.find(_.tpe.isRef(defn.FunctionClass(1))).flatMap { t =>
        getFunc1(t.tpe).map { case (t1, r, _) => (Some(t), t1, r) }
      }
  }

  private def getFunc1(tpe: Type)(implicit ctx: Context): Option[(Type, Type, Type)] =
    tpe.baseTypeWithArgs(defn.FunctionClass(1)) match {
      case func1 @ RefinedType(RefinedType(parent, _, t1), _, r) if (
        argTypes.contains(t1.typeSymbol) && retTypes.contains(r.typeSymbol)
      ) => Some((t1, r, func1))
      case _ => None
    }
}
