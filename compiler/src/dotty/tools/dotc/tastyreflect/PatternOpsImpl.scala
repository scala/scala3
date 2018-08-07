package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.ast.{Trees, tpd}
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Types

import scala.tasty.util.Show

trait PatternOpsImpl extends scala.tasty.reflect.PatternOps with TastyCoreImpl {

  // ----- Patterns -------------------------------------------------

  def PatternDeco(pattern: Pattern): PatternAPI = new PatternAPI {
    def pos(implicit ctx: Context): Position = pattern.pos
    def tpe(implicit ctx: Context): Types.Type = pattern.tpe.stripTypeVar
  }

  object Pattern extends PatternModule {

    object Value extends ValueExtractor {
      def unapply(x: Pattern)(implicit ctx: Context): Option[Term] = x match {
        case lit: tpd.Literal => Some(lit)
        case ref: tpd.RefTree if ref.isTerm => Some(ref)
        case ths: tpd.This => Some(ths)
        case _ => None
      }
    }

    object Bind extends BindExtractor {
      def unapply(x: Pattern)(implicit ctx: Context): Option[(String, Pattern)] = x match {
        case x: tpd.Bind if x.name.isTermName => Some(x.name.toString, x.body)
        case _ => None
      }
    }

    object Unapply extends UnapplyExtractor {
      def unapply(x: Pattern)(implicit ctx: Context): Option[(Term, List[Term], List[Pattern])] = x match {
        case Trees.UnApply(fun, implicits, patterns) => Some((fun, implicits, effectivePatterns(patterns)))
        case Trees.Typed(Trees.UnApply(fun, implicits, patterns), _) => Some((fun, implicits, effectivePatterns(patterns)))
        case _ => None
      }
      private def effectivePatterns(patterns: List[Pattern]): List[Pattern] = patterns match {
        case patterns0 :+ Trees.SeqLiteral(elems, _) => patterns0 ::: elems
        case _ => patterns
      }
    }

    object Alternative extends AlternativeExtractor {
      def unapply(x: Pattern)(implicit ctx: Context): Option[List[Pattern]] = x match {
        case x: tpd.Alternative => Some(x.trees)
        case _ => None
      }
    }

    object TypeTest extends TypeTestExtractor {
      def unapply(x: Pattern)(implicit ctx: Context): Option[TypeTree] = x match {
        case Trees.Typed(Trees.UnApply(_, _, _), _) => None
        case Trees.Typed(_, tpt) => Some(tpt)
        case _ => None
      }
    }

  }

}
