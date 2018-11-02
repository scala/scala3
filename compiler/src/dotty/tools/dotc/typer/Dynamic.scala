package dotty.tools
package dotc
package typer

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.{Name, TermName}
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Decorators._
import util.Positions._
import core.Symbols._
import core.Definitions
import ErrorReporting._
import dotty.tools.dotc.reporting.diagnostic.messages.ReassignmentToVal

object Dynamic {
  def isDynamicMethod(name: Name): Boolean =
    name == nme.applyDynamic || name == nme.selectDynamic || name == nme.updateDynamic || name == nme.applyDynamicNamed
}

/** Handles programmable member selections of `Dynamic` instances and values
 *  with structural types. Two functionalities:
 *
 * 1. Translates selection that does not typecheck according to the scala.Dynamic rules:
 *    foo.bar(baz) = quux                   ~~> foo.selectDynamic(bar).update(baz, quux)
 *    foo.bar = baz                         ~~> foo.updateDynamic("bar")(baz)
 *    foo.bar(x = bazX, y = bazY, baz, ...) ~~> foo.applyDynamicNamed("bar")(("x", bazX), ("y", bazY), ("", baz), ...)
 *    foo.bar(baz0, baz1, ...)              ~~> foo.applyDynamic(bar)(baz0, baz1, ...)
 *    foo.bar                               ~~> foo.selectDynamic(bar)
 *
 *  The first matching rule of is applied.
 *
 * 2. Translates member selections on structural types to calls of `selectDynamic`
 *    or `applyDynamic` on a `Selectable` instance. @See handleStructural.
 *
 */
trait Dynamic { self: Typer with Applications =>
  import Dynamic._
  import tpd._

  /** Translate selection that does not typecheck according to the normal rules into a applyDynamic/applyDynamicNamed.
   *    foo.bar(baz0, baz1, ...)                       ~~> foo.applyDynamic(bar)(baz0, baz1, ...)
   *    foo.bar[T0, ...](baz0, baz1, ...)              ~~> foo.applyDynamic[T0, ...](bar)(baz0, baz1, ...)
   *    foo.bar(x = bazX, y = bazY, baz, ...)          ~~> foo.applyDynamicNamed("bar")(("x", bazX), ("y", bazY), ("", baz), ...)
   *    foo.bar[T0, ...](x = bazX, y = bazY, baz, ...) ~~> foo.applyDynamicNamed[T0, ...]("bar")(("x", bazX), ("y", bazY), ("", baz), ...)
   */
  def typedDynamicApply(tree: untpd.Apply, pt: Type)(implicit ctx: Context): Tree = {
    def typedDynamicApply(qual: untpd.Tree, name: Name, selPos: Position, targs: List[untpd.Tree]): Tree = {
      def isNamedArg(arg: untpd.Tree): Boolean = arg match { case NamedArg(_, _) => true; case _ => false }
      val args = tree.args
      val dynName = if (args.exists(isNamedArg)) nme.applyDynamicNamed else nme.applyDynamic
      if (dynName == nme.applyDynamicNamed && untpd.isWildcardStarArgList(args))
        errorTree(tree, "applyDynamicNamed does not support passing a vararg parameter")
      else {
        def namedArgTuple(name: String, arg: untpd.Tree) = untpd.Tuple(List(Literal(Constant(name)), arg))
        def namedArgs = args.map {
          case NamedArg(argName, arg) => namedArgTuple(argName.toString, arg)
          case arg => namedArgTuple("", arg)
        }
        val args1 = if (dynName == nme.applyDynamic) args else namedArgs
        typedApply(untpd.Apply(coreDynamic(qual, dynName, name, selPos, targs), args1), pt)
      }
    }

     tree.fun match {
      case sel @ Select(qual, name) if !isDynamicMethod(name) =>
        typedDynamicApply(qual, name, sel.pos, Nil)
      case TypeApply(sel @ Select(qual, name), targs) if !isDynamicMethod(name) =>
        typedDynamicApply(qual, name, sel.pos, targs)
      case TypeApply(fun, targs) =>
        typedDynamicApply(fun, nme.apply, fun.pos, targs)
      case fun =>
        typedDynamicApply(fun, nme.apply, fun.pos, Nil)
    }
  }

  /** Translate selection that does not typecheck according to the normal rules into a selectDynamic.
   *    foo.bar          ~~> foo.selectDynamic(bar)
   *    foo.bar[T0, ...] ~~> foo.selectDynamic[T0, ...](bar)
   *
   *  Note: inner part of translation foo.bar(baz) = quux ~~> foo.selectDynamic(bar).update(baz, quux) is achieved
   *  through an existing transformation of in typedAssign [foo.bar(baz) = quux ~~> foo.bar.update(baz, quux)].
   */
  def typedDynamicSelect(tree: untpd.Select, targs: List[Tree], pt: Type)(implicit ctx: Context): Tree =
    typedApply(coreDynamic(tree.qualifier, nme.selectDynamic, tree.name, tree.pos, targs), pt)

  /** Translate selection that does not typecheck according to the normal rules into a updateDynamic.
   *    foo.bar = baz ~~> foo.updateDynamic(bar)(baz)
   */
  def typedDynamicAssign(tree: untpd.Assign, pt: Type)(implicit ctx: Context): Tree = {
    def typedDynamicAssign(qual: untpd.Tree, name: Name, selPos: Position, targs: List[untpd.Tree]): Tree =
      typedApply(untpd.Apply(coreDynamic(qual, nme.updateDynamic, name, selPos, targs), tree.rhs), pt)
    tree.lhs match {
      case sel @ Select(qual, name) if !isDynamicMethod(name) =>
        typedDynamicAssign(qual, name, sel.pos, Nil)
      case TypeApply(sel @ Select(qual, name), targs) if !isDynamicMethod(name) =>
        typedDynamicAssign(qual, name, sel.pos, targs)
      case _ =>
        errorTree(tree, ReassignmentToVal(tree.lhs.symbol.name))
    }
  }

  private def coreDynamic(qual: untpd.Tree, dynName: Name, name: Name, selPos: Position, targs: List[untpd.Tree])(implicit ctx: Context): untpd.Apply = {
    val select = untpd.Select(qual, dynName).withPos(selPos)
    val selectWithTypes =
      if (targs.isEmpty) select
      else untpd.TypeApply(select, targs)
    untpd.Apply(selectWithTypes, Literal(Constant(name.toString)))
  }

  /** Handle reflection-based dispatch for members of structural types.
   *
   *  Given `x.a`, where `x` is of (widened) type `T` (a value type or a nullary method type),
   *  and `x.a` is of type `U`, map `x.a` to the equivalent of:
   *
   *    (x: Selectable).selectDynamic("a").asInstanceOf[U]
   *
   *  Given `x.a(arg1, ..., argn)`, where `x.a` is of (widened) type (T1, ..., Tn)R,
   *  map `x.a(arg1, ..., argn)` to the equivalent of:
   *
   *    (x:selectable).applyDynamic("a", CT1, ..., CTn)(arg1, ..., argn).asInstanceOf[R]
   *
   *  where CT1, ..., CTn are the class tags representing the erasure of T1, ..., Tn.
   *
   *  It's an error if U is neither a value nor a method type, or a dependent method
   *  type.
   */
  def handleStructural(tree: Tree)(implicit ctx: Context): Tree = {

    def structuralCall(qual: Tree, name: Name, selectorName: TermName, ctags: List[Tree], args: Option[List[Tree]]) = {
      val selectable = adapt(qual, defn.SelectableType)

      // ($qual: Selectable).$selectorName("$name", ..$ctags)
      val base =
        untpd.Apply(
          untpd.TypedSplice(selectable.select(selectorName)).withPos(tree.pos),
          (Literal(Constant(name.toString)) :: ctags).map(untpd.TypedSplice(_)))

      val scall = args match {
        case None => base
        case Some(args) => untpd.Apply(base, args)
      }

      typed(scall)
    }

    def fail(name: Name, reason: String) =
      errorTree(tree, em"Structural access not allowed on method $name because it $reason")

    val tpe = tree.tpe.widen
    tree match {
      case Apply(fun @ Select(qual, name), args) =>
        val funTpe = fun.tpe.widen.asInstanceOf[MethodType]
        if (funTpe.isParamDependent)
          fail(name, i"has a method type with inter-parameter dependencies")
        else {
          val ctags = funTpe.paramInfos.map(pt =>
            implicitArgTree(defn.ClassTagType.appliedTo(pt :: Nil), tree.pos.endPos))
          structuralCall(qual, name, nme.applyDynamic, ctags, Some(args)).asInstance(tpe.resultType)
        }
      case Select(qual, name) if tpe.isValueType =>
        structuralCall(qual, name, nme.selectDynamic, Nil, None).asInstance(tpe)
      case Select(_, _) if !tpe.isParameterless =>
        // We return the tree unchanged; The structural call will be handled when we take care of the
        // enclosing application.
        tree
      case Select(_, name) if tpe.isInstanceOf[PolyType] =>
        fail(name, "is polymorphic")
      case Select(_, name) =>
        fail(name, i"has an unsupported type: ${tree.tpe.widen}")
    }
  }
}
