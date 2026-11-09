import scala.quoted.*

final class TestCallTree(inner: => Either[Any, IndexedSeq[TestCallTree]])

object Tests:
  transparent inline def apply(inline expr: Unit): TestCallTree =
    ${ applyImpl('expr) }

  private def applyImpl(body: Expr[Unit])(using Quotes): Expr[TestCallTree] =
    import quotes.reflect.*

    def testCallTreeExpr(setupStats: List[Statement]): Expr[TestCallTree] =
      val inner =
        Block(
          setupStats.dropRight(1),
          '{ Left(${setupStats.takeRight(1).head.asInstanceOf[Term].asExpr}) }.asTerm
        )
      '{ TestCallTree(${inner.asExprOf[Either[Any, IndexedSeq[TestCallTree]]]}) }

    object TestMethod:
      def unapply(tree: Tree): Option[Term] =
        Option(tree).collect { case t: Term => t.asExpr }.collect {
          case '{ ($name: String).-($body) } => body.asTerm
        }

    object Stats:
      def partition(stats: List[Statement]): (List[Apply], List[Statement]) =
        stats.partitionMap {
          case t: Term if TestMethod.unapply(t).isDefined => Left(t.asInstanceOf[Apply])
          case stmt                                       => Right(stmt)
        }
      def unapply(tree: Tree): Option[(List[Apply], List[Statement])] =
        tree match
          case Inlined(_, inlBindings, Stats(tests, testsBindings)) =>
            Some((tests, inlBindings ++ testsBindings))
          case Block(stats, expr) => Some(partition(stats :+ expr))
          case stmt: Statement    => Some(partition(stmt :: Nil))
          case _                  => None

    body.asTerm match
      case Stats(tests, _) =>
        val Some(testBody) = TestMethod.unapply(tests.head): @unchecked
        testBody match
          case Stats(Nil, setupStats) => testCallTreeExpr(setupStats)
          case other                  => testCallTreeExpr(List(other))
      case other =>
        report.errorAndAbort("bad Tests body: " + other.show)

extension (name: String)
  @annotation.compileTimeOnly("only inside Tests")
  def -(body: => Any): Unit = ()
