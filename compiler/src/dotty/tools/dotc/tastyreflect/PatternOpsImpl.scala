package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import dotty.tools.dotc.core.Contexts
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.StdNames.nme

trait PatternOpsImpl extends scala.tasty.reflect.PatternOps with CoreImpl {

  def ValueDeco(value: Value): Pattern.ValueAPI = new Pattern.ValueAPI {
    def value(implicit ctx: Context): Term = value
  }
  def BindDeco(bind: Bind): Pattern.BindAPI = new Pattern.BindAPI {
    def name(implicit ctx: Context): String = bind.name.toString
    def pattern(implicit ctx: Context): Pattern = bind.body
  }
  def UnapplyDeco(unapply: Unapply): Pattern.UnapplyAPI = new Pattern.UnapplyAPI {
    def fun(implicit ctx: Context): Term = unapply.fun
    def implicits(implicit ctx: Context): List[Term] = unapply.implicits
    def patterns(implicit ctx: Context): List[Pattern] = effectivePatterns(unapply.patterns)

    private def effectivePatterns(patterns: List[Pattern]): List[Pattern] = patterns match {
      case patterns0 :+ Trees.SeqLiteral(elems, _) => patterns0 ::: elems
      case _ => patterns
    }
  }
  def AlternativeDeco(alternatives: Alternatives): Pattern.AlternativesAPI = new Pattern.AlternativesAPI {
    def patterns(implicit ctx: Context): List[Pattern] = alternatives.trees
  }
  def TypeTestDeco(typeTest: TypeTest): Pattern.TypeTestAPI = new Pattern.TypeTestAPI {
    def tpt(implicit ctx: Context): TypeTree = typeTest.tpt
  }

  // ----- Patterns -------------------------------------------------

  def PatternDeco(pattern: Pattern): PatternAPI = new PatternAPI {
    def pos(implicit ctx: Context): Position = pattern.sourcePos
    def tpe(implicit ctx: Context): Type = pattern.tpe.stripTypeVar
    def symbol(implicit ctx: Context): Symbol = pattern.symbol
  }

  object Pattern extends PatternModule {

    object IsValue extends IsValueModule {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[Value] = pattern match {
        case lit: tpd.Literal => Some(lit)
        case ref: tpd.RefTree if ref.isTerm => Some(ref)
        case ths: tpd.This => Some(ths)
        case _ => None
      }
    }

    object Value extends ValueModule {
      def apply(term: Term)(implicit ctx: Context): Value = term match {
        case lit: tpd.Literal => lit
        case ref: tpd.RefTree if ref.isTerm => ref
        case ths: tpd.This => ths
      }
      def copy(original: Value)(term: Term)(implicit ctx: Context): Value = term match {
        case lit: tpd.Literal => tpd.cpy.Literal(original)(lit.const)
        case ref: tpd.RefTree if ref.isTerm => tpd.cpy.Ref(original.asInstanceOf[tpd.RefTree])(ref.name)
        case ths: tpd.This => tpd.cpy.This(original)(ths.qual)
      }
      def unapply(x: Pattern)(implicit ctx: Context): Option[Term] = IsValue.unapply(x)
    }

    object IsBind extends IsBindModule {
      def unapply(x: Pattern)(implicit ctx: Context): Option[Bind] = x match {
        case x: tpd.Bind if x.name.isTermName => Some(x)
        case _ => None
      }
    }

    object Bind extends BindModule {

      def copy(original: Bind)(name: String, pattern: Pattern)(implicit ctx: Context): Bind =
        tpd.cpy.Bind(original)(name.toTermName, pattern)

      def unapply(pattern: Pattern)(implicit ctx: Context): Option[(String, Pattern)] = pattern match {
        case IsBind(pattern) => Some((pattern.name.toString, pattern.body))
        case _ => None
      }
    }

    object IsUnapply extends IsUnapplyModule {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[Unapply] = pattern match {
        case pattern @ Trees.UnApply(_, _, _) => Some(pattern)
        case Trees.Typed(pattern @ Trees.UnApply(_, _, _), _) => Some(pattern)
        case _ => None
      }
    }

    object Unapply extends UnapplyModule {

      def copy(original: Unapply)(fun: Term, implicits: List[Term], patterns: List[Pattern])(implicit ctx: Context): Unapply =
        tpd.cpy.UnApply(original)(fun, implicits, patterns)

      def unapply(x: Pattern)(implicit ctx: Context): Option[(Term, List[Term], List[Pattern])] = x match {
        case IsUnapply(x) => Some((x.fun, x.implicits, UnapplyDeco(x).patterns))
        case _ => None
      }
    }

    object IsAlternatives extends IsAlternativesModule {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[Alternatives] = pattern match {
        case pattern: tpd.Alternative => Some(pattern)
        case _ => None
      }
    }

    object Alternatives extends AlternativesModule {
      def apply(patterns: List[Pattern])(implicit ctx: Context): Alternatives =
        tpd.Alternative(patterns)

      def copy(original: Alternatives)(patterns: List[Pattern])(implicit ctx: Context): Alternatives =
        tpd.cpy.Alternative(original)(patterns)

      def unapply(x: Pattern)(implicit ctx: Context): Option[List[Pattern]] = x match {
        case x: tpd.Alternative => Some(x.trees)
        case _ => None
      }
    }

    object IsTypeTest extends IsTypeTestModule {
      def unapply(pattern: Pattern)(implicit ctx: Context): Option[TypeTest] = pattern match {
        case Trees.Typed(_: tpd.UnApply, _) => None
        case pattern: tpd.Typed => Some(pattern)
        case _ => None
      }
    }

    object TypeTest extends TypeTestModule {
      def apply(tpt: TypeTree)(implicit ctx: Context): TypeTest =
        tpd.Typed(untpd.Ident(nme.WILDCARD).withType(tpt.tpe), tpt)

      def copy(original: TypeTest)(tpt: TypeTree)(implicit ctx: Context): TypeTest =
        tpd.cpy.Typed(original)(untpd.Ident(nme.WILDCARD).withType(tpt.tpe), tpt)

      def unapply(x: Pattern)(implicit ctx: Context): Option[TypeTree] = x match {
        case Trees.Typed(Trees.UnApply(_, _, _), _) => None
        case Trees.Typed(expr, tpt) => Some(tpt)
        case _ => None
      }
    }

  }

}
