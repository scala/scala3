import java.util.regex.Pattern
import scala.annotation.tailrec
import scala.quoted.*

object TinyMacro:
  inline def inspect(inline expr: Boolean): Unit = ${ inspectImpl('expr) }

  def inspectImpl(expr: Expr[Boolean])(using Quotes): Expr[Unit] =
    import quotes.reflect.*

    def flag(name: String): Int = name match
      case "CASE_INSENSITIVE" => Pattern.CASE_INSENSITIVE
      case "COMMENTS"         => Pattern.COMMENTS

    @tailrec
    def strip(term: Term): Term = term match
      case Inlined(_, _, inner) => strip(inner)
      case Typed(inner, _)      => strip(inner)
      case _                    => term

    def parseFlags(term: Term): List[Int] =
      @tailrec
      def loop(term: Term, acc: List[Int]): List[Int] = strip(term) match
        case Apply(term, List(Select(Ident("Pattern"), name))) => loop(term, acc :+ flag(name))
        case Select(term, "|")                                 => loop(term, acc)
        case Select(Ident("Pattern"), name)                    => acc :+ flag(name)
        // Upstream code changed in https://github.com/leviysoft/oolong/pull/61
        case Literal(IntConstant(n)) =>
          acc ++ (0 until 30).map(1 << _).filter(b => (n & b) != 0)

      loop(term, Nil)

    given FromExpr[Pattern] with
      def unapply(expr: Expr[Pattern])(using Quotes): Option[Pattern] =
        strip(expr.asTerm) match
          case Apply(
                Select(_, "compile"),
                List(Inlined(_, _, Typed(Literal(StringConstant(pattern)), _)), Literal(IntConstant(flags)))
              ) =>
            Some(Pattern.compile(pattern, flags))
          case Apply(
                Select(_, "compile"),
                List(Literal(StringConstant(pattern)), flagsTerm)
              ) =>
            Some(Pattern.compile(pattern, parseFlags(flagsTerm).reduce(_ | _)))
          case _ => None

    expr match
      case '{ ($pattern: Pattern).matcher(${ _: Expr[String] }).matches() } =>
        pattern.valueOrAbort
        '{ () }
      case _ =>
        report.errorAndAbort(expr.show)
