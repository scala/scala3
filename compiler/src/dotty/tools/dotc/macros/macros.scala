package dotty.tools.dotc

import core._
import Contexts.Context
import Symbols._
import transform.TreeTransforms.MiniPhaseTransform
import Types._
import Decorators._
import ast._
import util._
import StdNames._
import typer.ErrorReporting._
import reporting.diagnostic._
import config.Printers.{ macros => debug }

package object macros {
  import untpd._

  private class Proxy(clazz: Class[_], module: AnyRef) {
    def apply(name: String, params: Object*) = {
      val method = clazz.getDeclaredMethods().find(_.getName == name).get
      method.setAccessible(true)
      method.invoke(module, params: _*)
    }
  }

  private def forObject(qual: String)(implicit ctx: Context) = {
    val clazz = ctx.classloader.loadClass(qual + "$")
    val module = clazz.getField(nme.MODULE_INSTANCE_FIELD.toString).get(null)
    new Proxy(clazz, module)
  }

  /** Whether an Apply tree is quasiquote ?
   *  @param symbol the symbol of the function in the Apply tree
   *  @param tree   the Apply tree
   */
  def isQuasiquote(symbol: Symbol, tree: Tree)(implicit ctx: Context): Boolean =
    forObject("scala.meta.eden.quasiquote.package").apply("isQuasiquote", symbol, tree, ctx).asInstanceOf[Boolean]

  /** Expand a quasiquote tree */
  def expandQuasiquote(tree: Tree, isTerm: Boolean)(implicit ctx: Context): Tree = {
    forObject("scala.meta.eden.quasiquote.package").apply("expand", tree, Boolean.box(isTerm), ctx).asInstanceOf[Tree]
  }

  /** Expand annotation macros */
  def expandAnnotMacro(mdef: DefTree)(implicit ctx: Context): Tree = {
    if (
      !ctx.macrosEnabled ||
      !mdef.isInstanceOf[MemberDef] ||
      !macros.hasAnnotMacros(mdef.asInstanceOf[MemberDef])
    ) return mdef

    val tree = forObject("scala.meta.eden.expand.package").apply("expandAnnotMacro", mdef, ctx).asInstanceOf[Tree]
    tree match {
      case blk : Block =>   // introducing of same level symbols
        if (blk.stats.exists(!_.isInstanceOf[MemberDef]) || blk.expr != EmptyTree) {
          val errorMsg = "a definition can only expand to a list of definition. Actual:\n\n" + blk.show
          errorTree(mdef, new NoExplanation(errorMsg))
        }
        else {
          debug.println(i"macro expansion: $mdef expands to $blk")
          Thicket(blk.stats)
        }
      case _ => tree
    }
  }

  /** Does the given definition have annotation macros? */
  def hasAnnotMacros(mdef: MemberDef)(implicit ctx: Context): Boolean = {
    mdef.mods.annotations.filter(isAnnotMacro).headOption.nonEmpty
  }

  /** Is the given annotation an annotation macro? */
  def isAnnotMacro(ann: untpd.Tree)(implicit ctx: Context): Boolean = {
    import StdNames._

    val symbol = ctx.typer.typedAheadAnnotation(ann)
    if (!symbol.exists) return false

    val annMethod = symbol.info.decl(nme.apply)
    val annImplMethod = symbol.owner.info
      .decl((symbol.name + Transform.INLINE_SUFFIX).toTermName)
      .info
      .decl(nme.apply)

    symbol.typeRef <:< defn.StaticAnnotationType && annMethod.exists && annImplMethod.exists
  }

  /** Whether the given symbol is a def macro? */
  @inline
  def isDefMacro(symbol: Symbol)(implicit ctx: Context): Boolean =
    symbol.is(Flags.Macro) && !symbol.owner.is(Flags.Scala2x)

  /** Expand def macros */
  def expandDefMacro(tree: tpd.Tree)(implicit ctx: Context): Tree = {
    val res = forObject("scala.meta.eden.expand.package").apply("expandDefMacro", tree, ctx).asInstanceOf[Tree]
    debug.println(i"def macro expansion: $tree expands to $res")
    res
  }

  /** Transform macro definitions in class definition
   *
   *  @param tree  the tree that may contain macro definition, the tree may be a thicket
   *  @returns     the transformed tree
   *
   *  @note:  The returned tree NEEDs desugaring
   *
   *  Macro definition is transformed from:
   *
   *    class macros {
   *      inline def f[T](a: A)(b: B): C = meta {
   *        body
   *      }
   *    }
   *
   *  to:
   *
   *    class main {
   *      <macro> def f[T](a: A)(b: B): C = null
   *    }
   *
   *    object main$inline {
   *      def f(prefix: scala.meta.Term)(T: scala.meta.Type)(a: scala.meta.Term)(b: scala.meta.Term): scala.meta.Tree = body
   *    }
   */
  def transform(tree: Tree)(implicit ctx: Context): Tree = {
    tree match {
      case cdef: TypeDef if cdef.isClassDef =>
        Transform.transform(cdef)
      case mdef: ModuleDef =>
        Transform.transform(mdef)
      case thicket: Thicket =>
        Thicket(thicket.trees.map(transform))
      case _ =>
        tree
    }
  }
}
