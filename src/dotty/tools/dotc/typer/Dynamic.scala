package dotty.tools
package dotc
package typer

import dotty.tools.dotc.ast.Trees.NamedArg
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Decorators._

object Dynamic {
  def isDynamicMethod(name: Name): Boolean =
    name == nme.applyDynamic || name == nme.selectDynamic || name == nme.updateDynamic || name == nme.applyDynamicNamed
}

/** Translates selection that does not typecheck according to the scala.Dynamic rules:
 *    foo.bar(baz) = quux                   ~~> foo.selectDynamic(bar).update(baz, quux)
 *    foo.bar = baz                         ~~> foo.updateDynamic("bar")(baz)
 *    foo.bar(x = bazX, y = bazY, baz, ...) ~~> foo.applyDynamicNamed("bar")(("x", bazX), ("y", bazY), ("", baz), ...)
 *    foo.bar(baz0, baz1, ...)              ~~> foo.applyDynamic(bar)(baz0, baz1, ...)
 *    foo.bar                               ~~> foo.selectDynamic(bar)
 *
 *  The first matching rule of is applied.
 */
trait Dynamic { self: Typer with Applications =>

  /** Translate selection that does not typecheck according to the normal rules into a applyDynamic/applyDynamicNamed.
   *    foo.bar(baz0, baz1, ...)              ~~> foo.applyDynamic(bar)(baz0, baz1, ...)
   *    foo.bar(x = bazX, y = bazY, baz, ...) ~~> foo.applyDynamicNamed("bar")(("x", bazX), ("y", bazY), ("", baz), ...)
   */
  def typedDynamicApply(qual: untpd.Tree, name: Name, args: List[untpd.Tree], pt: Type)(original: untpd.Apply)(
      implicit ctx: Context): Tree = {
    def isNamedArg(arg: untpd.Tree): Boolean = arg match { case NamedArg(_, _) => true; case _ => false }
    val dynName = if (args.exists(isNamedArg)) nme.applyDynamicNamed else nme.applyDynamic
    if (dynName == nme.applyDynamicNamed && untpd.isWildcardStarArgList(args)) {
      ctx.error("applyDynamicNamed does not support passing a vararg parameter", original.pos)
      original.withType(ErrorType)
    } else {
      def namedArgTuple(name: String, arg: untpd.Tree) = untpd.Tuple(List(Literal(Constant(name)), arg))
      def namedArgs = args.map {
        case NamedArg(argName, arg) => namedArgTuple(argName.toString, arg)
        case arg => namedArgTuple("", arg)
      }
      val args1 = if (dynName == nme.applyDynamic) args else namedArgs
      typedApply(untpd.Apply(coreDynamic(qual, dynName, name), args1), pt)
    }
  }

  /** Translate selection that does not typecheck according to the normal rules into a selectDynamic.
   *    foo.bar ~~> foo.selectDynamic(bar)
   *
   *  Note: inner part of translation foo.bar(baz) = quux ~~> foo.selectDynamic(bar).update(baz, quux) is achieved
   *  through an existing transformation of in typedAssign [foo.bar(baz) = quux ~~> foo.bar.update(baz, quux)].
   */
  def typedDynamicSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree =
    typedApply(coreDynamic(tree.qualifier, nme.selectDynamic, tree.name), pt)

  /** Translate selection that does not typecheck according to the normal rules into a updateDynamic.
   *    foo.bar = baz ~~> foo.updateDynamic(bar)(baz)
   */
  def typedDynamicAssign(qual: untpd.Tree, name: Name, rhs: untpd.Tree, pt: Type)(implicit ctx: Context): Tree =
    typedApply(untpd.Apply(coreDynamic(qual, nme.updateDynamic, name), rhs), pt)

  private def coreDynamic(qual: untpd.Tree, dynName: Name, name: Name)(implicit ctx: Context): untpd.Apply =
    untpd.Apply(untpd.Select(qual, dynName), Literal(Constant(name.toString)))
}
