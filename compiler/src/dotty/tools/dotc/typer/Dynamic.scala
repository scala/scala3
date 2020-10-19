package dotty.tools
package dotc
package typer

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Names.{Name, TermName}
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Decorators._
import util.Spans._
import core.Symbols._
import core.Definitions
import ErrorReporting._
import reporting._
import util.Lst
import util.Lst.{NIL, +:, toLst}

object Dynamic {
  def isDynamicMethod(name: Name): Boolean =
    name == nme.applyDynamic || name == nme.selectDynamic || name == nme.updateDynamic || name == nme.applyDynamicNamed
}

object DynamicUnapply {
  def unapply(tree: tpd.Tree): Option[List[tpd.Tree]] = tree match
    case TypeApply(Select(qual, name), _) if name == nme.asInstanceOfPM =>
      unapply(qual)
    case Apply(Apply(Select(selectable, fname), Lst(Literal(Constant(name)), ctag)), _ +: implicits)
    if fname == nme.applyDynamic && (name == "unapply" || name == "unapplySeq") =>
      Some(selectable :: ctag :: implicits.toScalaList)
    case _ =>
      None
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
trait Dynamic {
  self: Typer & Applications =>

  import Dynamic._
  import tpd._

  /** Translate selection that does not typecheck according to the normal rules into a applyDynamic/applyDynamicNamed.
   *    foo.bar(baz0, baz1, ...)                       ~~> foo.applyDynamic(bar)(baz0, baz1, ...)
   *    foo.bar[T0, ...](baz0, baz1, ...)              ~~> foo.applyDynamic[T0, ...](bar)(baz0, baz1, ...)
   *    foo.bar(x = bazX, y = bazY, baz, ...)          ~~> foo.applyDynamicNamed("bar")(("x", bazX), ("y", bazY), ("", baz), ...)
   *    foo.bar[T0, ...](x = bazX, y = bazY, baz, ...) ~~> foo.applyDynamicNamed[T0, ...]("bar")(("x", bazX), ("y", bazY), ("", baz), ...)
   */
  def typedDynamicApply(tree: untpd.Apply, isInsertedApply: Boolean, pt: Type)(using Context): Tree = {
    def typedDynamicApply(qual: untpd.Tree, name: Name, selSpan: Span, targs: Lst[untpd.Tree]): Tree = {
      def isNamedArg(arg: untpd.Tree): Boolean = arg match { case NamedArg(_, _) => true; case _ => false }
      val args = tree.args
      val dynName = if (args.exists(isNamedArg)) nme.applyDynamicNamed else nme.applyDynamic
      if (dynName == nme.applyDynamicNamed && untpd.isWildcardStarArgList(args))
        errorTree(tree, "applyDynamicNamed does not support passing a vararg parameter")
      else {
        def namedArgTuple(name: String, arg: untpd.Tree) = untpd.Tuple(Lst(Literal(Constant(name)), arg))
        def namedArgs = args.map {
          case NamedArg(argName, arg) => namedArgTuple(argName.toString, arg)
          case arg => namedArgTuple("", arg)
        }
        val args1 = if (dynName == nme.applyDynamic) args else namedArgs
        typedApply(untpd.Apply(coreDynamic(qual, dynName, name, selSpan, targs), args1), pt)
      }
    }

    if (isInsertedApply) {
      tree.fun match {
        case TypeApply(fun, targs) =>
          typedDynamicApply(fun, nme.apply, fun.span, targs)
        case fun =>
          typedDynamicApply(fun, nme.apply, fun.span, NIL)
      }
    } else {
      tree.fun match {
        case sel @ Select(qual, name) if !isDynamicMethod(name) =>
          typedDynamicApply(qual, name, sel.span, NIL)
        case TypeApply(sel @ Select(qual, name), targs) if !isDynamicMethod(name) =>
          typedDynamicApply(qual, name, sel.span, targs)
        case _ =>
          errorTree(tree, em"Dynamic insertion not applicable")
      }
    }
  }

  /** Translate selection that does not typecheck according to the normal rules into a selectDynamic.
   *    foo.bar          ~~> foo.selectDynamic(bar)
   *    foo.bar[T0, ...] ~~> foo.selectDynamic[T0, ...](bar)
   *
   *  Note: inner part of translation foo.bar(baz) = quux ~~> foo.selectDynamic(bar).update(baz, quux) is achieved
   *  through an existing transformation of in typedAssign [foo.bar(baz) = quux ~~> foo.bar.update(baz, quux)].
   */
  def typedDynamicSelect(tree: untpd.Select, targs: Lst[Tree], pt: Type)(using Context): Tree =
    typedApply(coreDynamic(tree.qualifier, nme.selectDynamic, tree.name, tree.span, targs), pt)

  /** Translate selection that does not typecheck according to the normal rules into a updateDynamic.
   *    foo.bar = baz ~~> foo.updateDynamic(bar)(baz)
   */
  def typedDynamicAssign(tree: untpd.Assign, pt: Type)(using Context): Tree = {
    def typedDynamicAssign(qual: untpd.Tree, name: Name, selSpan: Span, targs: Lst[untpd.Tree]): Tree =
      typedApply(untpd.Apply(coreDynamic(qual, nme.updateDynamic, name, selSpan, targs), tree.rhs), pt)
    tree.lhs match {
      case sel @ Select(qual, name) if !isDynamicMethod(name) =>
        typedDynamicAssign(qual, name, sel.span, NIL)
      case TypeApply(sel @ Select(qual, name), targs) if !isDynamicMethod(name) =>
        typedDynamicAssign(qual, name, sel.span, targs)
      case _ =>
        errorTree(tree, ReassignmentToVal(tree.lhs.symbol.name))
    }
  }

  private def coreDynamic(qual: untpd.Tree, dynName: Name, name: Name, selSpan: Span, targs: Lst[untpd.Tree])(using Context): untpd.Apply = {
    val select = untpd.Select(qual, dynName).withSpan(selSpan)
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
   *  ```scala
   *  x1.selectDynamic("a").asInstanceOf[U]
   *  ```
   *  where `x1` is `x` adapted to `Selectable`.
   *
   *  Given `x.a(a11, ..., a1n)...(aN1, ..., aNn)`, where `x.a` is of (widened) type
   *  `(T11, ..., T1n)...(TN1, ..., TNn): R`, it is desugared to:
   *
   *  ```scala
   *  x1.applyDynamic("a")(a11, ..., a1n, ..., aN1, ..., aNn)
   *    .asInstanceOf[R]
   *  ```
   *  If this call resolves to an `applyDynamic` method that takes a `ClassTag[?]*` as second
   *  parameter, we further rewrite this call to
   *  scala```
   *  x1.applyDynamic("a", CT11, ..., CT1n, ..., CTN1, ... CTNn)
   *                  (a11, ..., a1n, ..., aN1, ..., aNn)
   *    .asInstanceOf[R]
   *  ```
   *  where CT11, ..., CTNn are the class tags representing the erasure of T11, ..., TNn.
   *
   *  It's an error if U is neither a value nor a method type, or a dependent method
   *  type.
   */
  def handleStructural(tree: Tree)(using Context): Tree = {
    val (fun @ Select(qual, name), targs, vargss) = decomposeCall(tree)

    def structuralCall(selectorName: TermName, ctags: => List[Tree]) = {
      val selectable = adapt(qual, defn.SelectableClass.typeRef)

      // ($qual: Selectable).$selectorName("$name")
      val base =
        untpd.Apply(
          untpd.TypedSplice(selectable.select(selectorName)).withSpan(fun.span),
          Lst(Literal(Constant(name.toString))).map(untpd.TypedSplice(_)))

      val scall =
        if (vargss.isEmpty) base
        else untpd.Apply(base, vargss.flatten.map(untpd.TypedSplice(_)))

      // If function is an `applyDynamic` that takes a ClassTag* parameter,
      // add `ctags`.
      def addClassTags(tree: Tree): Tree = tree match
        case Apply(fn: Apply, args) =>
          cpy.Apply(tree)(addClassTags(fn), args)
        case Apply(fn @ Select(_, nme.applyDynamic), Lst(nameArg, _)) =>
          fn.tpe.widen match
            case mt: MethodType => mt.paramInfos match
              case _ :: ctagsParam :: Nil
              if ctagsParam.isRepeatedParam
                 && ctagsParam.argInfos.head.isRef(defn.ClassTagClass) =>
                  val ctagType = defn.ClassTagClass.typeRef.appliedTo(TypeBounds.empty)
                  cpy.Apply(tree)(fn,
                    Lst(nameArg, seqToRepeated(SeqLiteral(ctags.toLst, TypeTree(ctagType)))))
              case _ => tree
            case other => tree
        case _ => tree
      addClassTags(typed(scall))
    }

    def fail(name: Name, reason: String) =
      errorTree(tree, em"Structural access not allowed on method $name because it $reason")

    fun.tpe.widen match {
      case tpe: ValueType =>
        structuralCall(nme.selectDynamic, Nil).cast(tpe)

      case tpe: MethodType =>
        def isDependentMethod(tpe: Type): Boolean = tpe match {
          case tpe: MethodType =>
            tpe.isParamDependent ||
            tpe.isResultDependent ||
            isDependentMethod(tpe.resultType)
          case _ =>
            false
        }

        if (isDependentMethod(tpe))
          fail(name, i"has a method type with inter-parameter dependencies")
        else {
          def ctags = tpe.paramInfoss.flatten.map(pt =>
            implicitArgTree(defn.ClassTagClass.typeRef.appliedTo(pt.widenDealias :: Nil), fun.span.endPos))
          structuralCall(nme.applyDynamic, ctags).cast(tpe.finalResultType)
        }

      // (@allanrenucci) I think everything below is dead code
      case _: PolyType =>
        fail(name, "is polymorphic")
      case tpe =>
        fail(name, i"has an unsupported type: $tpe")
    }
  }
}
