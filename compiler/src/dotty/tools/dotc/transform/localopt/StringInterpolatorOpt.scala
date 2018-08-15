package dotty.tools.dotc.transform.localopt

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.util.Result

/**
  * MiniPhase to transform s and raw string interpolators from using StringContext to string
  * concatenation. Since string concatenation uses the Java String builder, we get a performance
  * improvement in terms of these two interpolators.
  *
  * More info here:
  * https://medium.com/@dkomanov/scala-string-interpolation-performance-21dc85e83afd
  */
class StringInterpolatorOpt extends MiniPhase {
  import tpd._

  override def phaseName: String = "stringInterpolatorOpt"

  /** Matches a list of constant literals */
  private object Literals {
    def unapply(tree: SeqLiteral)(implicit ctx: Context): Result[List[Literal]] = {
      tree.elems match {
        case literals if literals.forall(_.isInstanceOf[Literal]) =>
          Result(literals.map(_.asInstanceOf[Literal]))
        case _ => Result.empty
      }
    }
  }

  private object StringContextApply {
    def unapply(tree: Select)(implicit ctx: Context): Boolean = {
      tree.symbol.eq(defn.StringContextModule_apply) &&
      tree.qualifier.symbol.eq(defn.StringContextModule)
    }
  }

  /** Matches an s or raw string interpolator */
  private object SOrRawInterpolator {
    def unapply(tree: Tree)(implicit ctx: Context): Result[(List[Literal], List[Tree])] = {
      tree match {
        case Apply(Select(Apply(StringContextApply(), List(Literals(strs))), _),
        List(SeqLiteral(elems, _))) if elems.length == strs.length - 1 =>
          Result(strs, elems)
        case _ => Result.empty
      }
    }
  }

  /**
    * Match trees that resemble s and raw string interpolations. In the case of the s
    * interpolator, escapes the string constants. Exposes the string constants as well as
    * the variable references.
    */
  private object StringContextIntrinsic {
    def unapply(tree: Apply)(implicit ctx: Context): Result[(List[Literal], List[Tree])] = {
      tree match {
        case SOrRawInterpolator(strs, elems) =>
          if (tree.symbol == defn.StringContextRaw) Result(strs, elems)
          else { // tree.symbol == defn.StringContextS
            try {
              val escapedStrs = strs.map { str =>
                val escapedValue = StringContext.processEscapes(str.const.stringValue)
                cpy.Literal(str)(Constant(escapedValue))
              }
              Result(escapedStrs, elems)
            } catch {
              case _: StringContext.InvalidEscapeException => Result.empty
            }
          }
        case _ => Result.empty
      }
    }
  }

  override def transformApply(tree: Apply)(implicit ctx: Context): Tree = {
    val sym = tree.symbol
    val isInterpolatedMethod = // Test names first to avoid loading scala.StringContext if not used
      (sym.name == nme.raw_ && sym.eq(defn.StringContextRaw)) ||
      (sym.name == nme.s && sym.eq(defn.StringContextS))
    if (isInterpolatedMethod) transformInterpolator(tree)
    else tree
  }

  private def transformInterpolator(tree: Tree)(implicit ctx: Context): Tree = {
    tree match {
      case StringContextIntrinsic(strs: List[Literal], elems: List[Tree]) =>
        val stri = strs.iterator
        val elemi = elems.iterator
        var result: Tree = stri.next
        def concat(tree: Tree): Unit = {
          result = result.select(defn.String_+).appliedTo(tree)
        }
        while (elemi.hasNext) {
          concat(elemi.next)
          val str = stri.next
          if (!str.const.stringValue.isEmpty) concat(str)
        }
        result
      case _ => tree
    }
  }
}
