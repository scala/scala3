package dotty.tools.dotc.transform.localopt

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types.MethodType
import dotty.tools.dotc.transform.MegaPhase.MiniPhase

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

  override def checkPostCondition(tree: tpd.Tree)(implicit ctx: Context): Unit = {
    tree match {
      case tree: RefTree =>
        val sym = tree.symbol
        assert(sym != defn.StringContext_raw && sym != defn.StringContext_s,
          i"$tree in ${ctx.owner.showLocated} should have been rewritten by phase $phaseName")
      case _ =>
    }
  }

  /** Matches a list of constant literals */
  private object Literals {
    def unapply(tree: SeqLiteral)(implicit ctx: Context): Option[List[Literal]] = {
      tree.elems match {
        case literals if literals.forall(_.isInstanceOf[Literal]) =>
          Some(literals.map(_.asInstanceOf[Literal]))
        case _ => None
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
    def unapply(tree: Tree)(implicit ctx: Context): Option[(List[Literal], List[Tree])] = {
      tree match {
        case Apply(Select(Apply(StringContextApply(), List(Literals(strs))), _),
        List(SeqLiteral(elems, _))) if elems.length == strs.length - 1 =>
          Some(strs, elems)
        case _ => None
      }
    }
  }

  /**
    * Match trees that resemble s and raw string interpolations. In the case of the s
    * interpolator, escapes the string constants. Exposes the string constants as well as
    * the variable references.
    */
  private object StringContextIntrinsic {
    def unapply(tree: Apply)(implicit ctx: Context): Option[(List[Literal], List[Tree])] = {
      tree match {
        case SOrRawInterpolator(strs, elems) =>
          if (tree.symbol == defn.StringContext_raw) Some(strs, elems)
          else { // tree.symbol == defn.StringContextS
            try {
              val escapedStrs = strs.map { str =>
                val escapedValue = StringContext.processEscapes(str.const.stringValue)
                cpy.Literal(str)(Constant(escapedValue))
              }
              Some(escapedStrs, elems)
            } catch {
              case _: StringContext.InvalidEscapeException => None
            }
          }
        case _ => None
      }
    }
  }

  override def transformApply(tree: Apply)(implicit ctx: Context): Tree = {
    val sym = tree.symbol
    val isInterpolatedMethod = // Test names first to avoid loading scala.StringContext if not used
      (sym.name == nme.raw_ && sym.eq(defn.StringContext_raw)) ||
      (sym.name == nme.s && sym.eq(defn.StringContext_s))
    if (isInterpolatedMethod)
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
        // Starting with Scala 2.13, s and raw are macros in the standard
        // library, so we need to expand them manually.
        // sc.s(args)    -->   standardInterpolator(processEscapes, args, sc.parts)
        // sc.raw(args)  -->   standardInterpolator(x => x,         args, sc.parts)
        case Apply(intp, args :: Nil) =>
          val pre = intp match {
            case Select(pre, _) => pre
            case intp: Ident => tpd.desugarIdentPrefix(intp)
          }
          val isRaw = sym eq defn.StringContext_raw
          val stringToString = defn.StringContextModule_processEscapes.info.asInstanceOf[MethodType]

          val process = tpd.Lambda(stringToString, args =>
            if (isRaw) args.head else ref(defn.StringContextModule_processEscapes).appliedToArgs(args))

          evalOnce(pre) { sc =>
            val parts = sc.select(defn.StringContext_parts)

            ref(defn.StringContextModule_standardInterpolator)
              .appliedToArgs(List(process, args, parts))
          }
      }
    else
      tree
  }
}
