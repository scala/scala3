package dotty.tools
package dotc
package transform

import TreeTransforms.{ MiniPhaseTransform, TransformerInfo }
import core._
import Contexts.Context, Types._, Decorators._, Symbols._, DenotTransformers._
import Denotations._, SymDenotations._, Scopes._, StdNames._, NameOps._, Names._
import ast.tpd

class SpecializeFunctions extends MiniPhaseTransform with DenotTransformer {
  import ast.tpd._

  val phaseName = "specializeFunction1"

  // Setup ---------------------------------------------------------------------
  private[this] val functionName = "JFunction".toTermName
  private[this] val functionPkg  = "scala.compat.java8.".toTermName
  private[this] var argTypes: Set[Symbol] = _
  private[this] var retTypes: Set[Symbol] = _

  override def prepareForUnit(tree: Tree)(implicit ctx: Context) = {
    retTypes = Set(defn.UnitClass,
                   defn.BooleanClass,
                   defn.IntClass,
                   defn.FloatClass,
                   defn.LongClass,
                   defn.DoubleClass,
                   /* only for Function0: */
                   defn.ByteClass,
                   defn.ShortClass,
                   defn.CharClass)

    argTypes = Set(defn.IntClass,
                   defn.LongClass,
                   defn.DoubleClass,
                   /* only for Function1: */
                   defn.FloatClass)
    this
  }

  // Transformations -----------------------------------------------------------

  /** Transforms all classes extending `Function1[-T1, +R]` so that
    * they instead extend the specialized version `JFunction$mp...`
    */
  def transform(ref: SingleDenotation)(implicit ctx: Context) = ref match {
    case cref @ ShouldTransformDenot(targets) => {
      val specializedSymbols: Map[Symbol, (Symbol, Symbol)] = (for (SpecializationTarget(target, args, ret, original) <- targets) yield {
        val arity = args.length
        val specializedParent = ctx.getClassIfDefined {
          functionPkg ++ specializedName(functionName ++ arity, args, ret)
        }

        val specializedApply: Symbol = {
          val specializedMethodName = specializedName(nme.apply, args, ret)
          ctx.newSymbol(
            cref.symbol,
            specializedMethodName,
            Flags.Override | Flags.Method,
            specializedParent.info.decls.lookup(specializedMethodName).info
          )
        }

        original -> (specializedParent, specializedApply)
      }).toMap

      def specializeApplys(scope: Scope): Scope = {
        val alteredScope = scope.cloneScope
        specializedSymbols.values.foreach { case (_, apply) =>
          alteredScope.enter(apply)
        }
        alteredScope
      }

      def replace(in: List[TypeRef]): List[TypeRef] =
        in.map { tref =>
          val sym = tref.symbol
          specializedSymbols.get(sym).map { case (specializedParent, _) =>
            specializedParent.typeRef
          }
          .getOrElse(tref)
        }

      val ClassInfo(prefix, cls, parents, decls, info) = cref.classInfo
      val newParents = replace(parents)
      val newInfo = ClassInfo(prefix, cls, newParents, specializeApplys(decls), info)
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
      case tmpl @ ShouldTransformTree(targets) => {
        val symbolMap = (for ((tree, SpecializationTarget(target, args, ret, orig)) <- targets) yield {
          val arity = args.length
          val specializedParent = TypeTree {
            ctx.requiredClassRef(functionPkg ++ specializedName(functionName ++ arity, args, ret))
          }
          val specializedMethodName = specializedName(nme.apply, args, ret)
          val specializedApply = ctx.owner.info.decls.lookup(specializedMethodName)

          if (specializedApply.exists)
            Some(orig -> (specializedParent, specializedApply.asTerm))
          else None
        }).flatten.toMap

        val body0 = tmpl.body.foldRight(List.empty[Tree]) {
          case (tree: DefDef, acc) if tree.name == nme.apply => {
            val inheritedFrom =
              tree.symbol.allOverriddenSymbols
              .map(_.owner)
              .map(symbolMap.get)
              .flatten
              .toList
              .headOption

            inheritedFrom.map { case (parent, apply) =>
              val forwardingBody = tpd
                .ref(apply)
                .appliedToArgs(tree.vparamss.head.map(vparam => ref(vparam.symbol)))

              val applyWithForwarding = cpy.DefDef(tree)(rhs = forwardingBody)

              val specializedApplyDefDef =
                polyDefDef(apply, trefs => vrefss => {
                  tree.rhs
                    .changeOwner(tree.symbol, apply)
                    .subst(tree.vparamss.flatten.map(_.symbol), vrefss.flatten.map(_.symbol))
                })

              applyWithForwarding :: specializedApplyDefDef :: acc
            }
            .getOrElse(tree :: acc)
          }
          case (tree, acc) => tree :: acc
        }

        val specializedParents = tree.parents.map { t =>
          symbolMap
            .get(t.symbol)
            .map { case (newSym, _) => newSym }
            .getOrElse(t)
        }

        cpy.Template(tmpl)(parents = specializedParents, body = body0)
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

  private def specializedName(name: Name, args: List[Type], ret: Type)(implicit ctx: Context): Name = {
    val typeParams = args :+ ret
    name.specializedFor(typeParams, typeParams.map(_.typeSymbol.name), Nil, Nil)
  }

  // Extractors ----------------------------------------------------------------
  private object ShouldTransformDenot {
    def unapply(cref: ClassDenotation)(implicit ctx: Context): Option[Seq[SpecializationTarget]] =
      if (!cref.classParents.map(_.symbol).exists(defn.isPlainFunctionClass)) None
      else Some(getSpecTargets(cref.typeRef))
  }

  private object ShouldTransformTree {
    def unapply(tree: Template)(implicit ctx: Context): Option[Seq[(Tree, SpecializationTarget)]] = {
      val treeToTargets = tree.parents
        .map(t => (t, getSpecTargets(t.tpe)))
        .filter(_._2.nonEmpty)
        .map { case (t, xs) => (t, xs.head) }

      if (treeToTargets.isEmpty) None else Some(treeToTargets)
    }
  }

  private case class SpecializationTarget(target: Symbol,
                                          params: List[Type],
                                          ret: Type,
                                          original: Symbol)

  /** Gets all valid specialization targets on `tpe`, allowing multiple
   *  implementations of FunctionX traits
   */
  private def getSpecTargets(tpe: Type)(implicit ctx: Context): List[SpecializationTarget] = {
    val functionParents =
      tpe.classSymbols.iterator
        .flatMap(_.baseClasses)
        .filter(defn.isPlainFunctionClass)

    val tpeCls = tpe.widenDealias
    functionParents.map { sym =>
      val typeParams = tpeCls.baseArgTypes(sym)
      val args = typeParams.init
      val ret = typeParams.last

      val interfaceName =
        (functionName ++ args.length)
        .specializedFor(typeParams, typeParams.map(_.typeSymbol.name), Nil, Nil)

      val interface = ctx.getClassIfDefined(functionPkg ++ interfaceName)

      if (interface.exists) Some {
        SpecializationTarget(interface, args, ret, sym)
      }
      else None
    }
    .flatten.toList
  }
}
