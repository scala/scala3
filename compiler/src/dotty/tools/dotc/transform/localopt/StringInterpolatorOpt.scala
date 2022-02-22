package dotty.tools.dotc
package transform.localopt

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.transform.MegaPhase.MiniPhase
import dotty.tools.dotc.typer.ConstFold

/** MiniPhase to transform s and raw string interpolators from using StringContext to string
 *  concatenation. Since string concatenation uses the Java String builder, we get a performance
 *  improvement in terms of these two interpolators.
 *
 *  More info here:
 *  https://medium.com/@dkomanov/scala-string-interpolation-performance-21dc85e83afd
 */
class StringInterpolatorOpt extends MiniPhase:
  import tpd.*

  override def phaseName: String = StringInterpolatorOpt.name

  override def description: String = StringInterpolatorOpt.description

  override def checkPostCondition(tree: tpd.Tree)(using Context): Unit =
    tree match
      case tree: RefTree =>
        val sym = tree.symbol
        assert(sym != defn.StringContext_raw && sym != defn.StringContext_s && sym != defn.StringContext_f,
          i"$tree in ${ctx.owner.showLocated} should have been rewritten by phase $phaseName")
      case _ =>

  /** Matches a list of constant literals */
  private object Literals:
    def unapply(tree: SeqLiteral)(using Context): Option[List[Literal]] =
      tree.elems match
        case literals if literals.forall(_.isInstanceOf[Literal]) => Some(literals.map(_.asInstanceOf[Literal]))
        case _ => None

  private object StringContextApply:
    def unapply(tree: Select)(using Context): Boolean =
      (tree.symbol eq defn.StringContextModule_apply) && (tree.qualifier.symbol eq defn.StringContextModule)

  /** Matches an s or raw string interpolator */
  private object SOrRawInterpolator:
    def unapply(tree: Tree)(using Context): Option[(List[Literal], List[Tree])] =
      tree match
        case Apply(Select(Apply(StringContextApply(), List(Literals(strs))), _), List(SeqLiteral(elems, _)))
        if elems.length == strs.length - 1 => Some(strs, elems)
        case _ => None

  //Extract the position from InvalidUnicodeEscapeException
  //which due to bincompat reasons is unaccessible.
  //TODO: remove once there is less restrictive bincompat
  private object InvalidEscapePosition:
    def unapply(t: Throwable): Option[Int] = t match
      case iee: StringContext.InvalidEscapeException => Some(iee.index)
      case iae: IllegalArgumentException => iae.getMessage() match
        case s"""invalid unicode escape at index $index of $_""" => index.toIntOption
        case _ => None
      case _ => None

  /** Match trees that resemble s and raw string interpolations. In the case of the s
   *  interpolator, escapes the string constants. Exposes the string constants as well as
   *  the variable references.
   */
  private object StringContextIntrinsic:
    def unapply(tree: Apply)(using Context): Option[(List[Literal], List[Tree])] =
      tree match
        case SOrRawInterpolator(strs, elems) =>
          if tree.symbol == defn.StringContext_raw then Some(strs, elems)
          else // tree.symbol == defn.StringContextS
            import dotty.tools.dotc.util.SourcePosition
            var stringPosition: SourcePosition = null
            try
              val escapedStrs = strs.map { str =>
                stringPosition = str.sourcePos
                val escaped = StringContext.processEscapes(str.const.stringValue)
                cpy.Literal(str)(Constant(escaped))
              }
              Some(escapedStrs, elems)
            catch
              case t @ InvalidEscapePosition(p) =>
                val errorSpan = stringPosition.span.startPos.shift(p)
                val errorPosition = stringPosition.withSpan(errorSpan)
                report.error(t.getMessage() + "\n", errorPosition)
                None
        case _ => None

  override def transformApply(tree: Apply)(using Context): Tree =
    def mkConcat(strs: List[Literal], elems: List[Tree]): Tree =
      val stri = strs.iterator
      val elemi = elems.iterator
      var result: Tree = stri.next
      def concat(tree: Tree): Unit =
        result = result.select(defn.String_+).appliedTo(tree).withSpan(tree.span)
      while elemi.hasNext
      do
        concat(elemi.next)
        val str = stri.next
        if !str.const.stringValue.isEmpty then concat(str)
      result
    end mkConcat
    val sym = tree.symbol
    // Test names first to avoid loading scala.StringContext if not used, and common names first
    val isInterpolatedMethod =
      sym.name match
        case nme.s    => sym eq defn.StringContext_s
        case nme.raw_ => sym eq defn.StringContext_raw
        case nme.f    => sym eq defn.StringContext_f
        case _        => false
    // Perform format checking and normalization, then make it StringOps(fmt).format(args1) with tweaked args
    def transformF(fun: Tree, args: Tree): Tree =
      val (fmt, args1) = FormatInterpolatorTransform.checked(fun, args)
      resolveConstructor(defn.StringOps.typeRef, List(fmt))
        .select(nme.format)
        .appliedTo(args1)
    // Starting with Scala 2.13, s and raw are macros in the standard
    // library, so we need to expand them manually.
    // sc.s(args)    -->   standardInterpolator(processEscapes, args, sc.parts)
    // sc.raw(args)  -->   standardInterpolator(x => x,         args, sc.parts)
    def transformS(fun: Tree, args: Tree, isRaw: Boolean): Tree =
      val pre = fun match
        case Select(pre, _) => pre
        case intp: Ident    => tpd.desugarIdentPrefix(intp)
      val stringToString = defn.StringContextModule_processEscapes.info.asInstanceOf[MethodType]
      val process = tpd.Lambda(stringToString, args =>
        if isRaw then args.head else ref(defn.StringContextModule_processEscapes).appliedToTermArgs(args)
      )
      evalOnce(pre) { sc =>
        val parts = sc.select(defn.StringContext_parts)
        ref(defn.StringContextModule_standardInterpolator)
          .appliedToTermArgs(List(process, args, parts))
      }
    end transformS
    // begin transformApply
    if isInterpolatedMethod then
      (tree: @unchecked) match
        case StringContextIntrinsic(strs: List[Literal], elems: List[Tree]) =>
          mkConcat(strs, elems)
        case Apply(intp, args :: Nil) =>
          if sym eq defn.StringContext_f then transformF(intp, args)
          else transformS(intp, args, isRaw = sym eq defn.StringContext_raw)
    else
      tree.tpe match
        case _: ConstantType => tree
        case _ =>
          ConstFold.Apply(tree).tpe match
            case ConstantType(x) => Literal(x).withSpan(tree.span).ensureConforms(tree.tpe)
            case _ => tree

  override def transformSelect(tree: Select)(using Context): Tree =
    ConstFold.Select(tree).tpe match
      case ConstantType(x) => Literal(x).withSpan(tree.span).ensureConforms(tree.tpe)
      case _ => tree

object StringInterpolatorOpt:
  val name: String = "stringInterpolatorOpt"
  val description: String = "optimize s, f and raw string interpolators"
