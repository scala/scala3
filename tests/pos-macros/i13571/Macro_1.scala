import scala.quoted.*

inline def checked2[A](inline n: A): A =
  ${ checkedImpl2[A]('{n}) }

private def checkedImpl2[A](n: Expr[A])(using Quotes, Type[A]): Expr[A] =
  import quotes.reflect.*
  val tree: Term = n.asTerm
  val acc = new TreeMap:
    override def transformTerm(tree: Term)(owner: Symbol): Term =
      tree match
        case Apply(Select(x, "*"), List(y)) =>
          given Quotes = owner.asQuotes
          '{
            val xt = ${x.asExprOf[Long]}
            xt
          }.asTerm
        case _ =>
          super.transformTerm(tree)(owner)
  acc.transformTerm(tree)(Symbol.spliceOwner).asExprOf[A]
