//> using options -Yimplicit-as-given
import scala.quoted._
object Macro:
  inline def test[I] = ${testImpl[I]}
  def testImpl[I: Type](using Quotes): Expr[Unit] =
    import quotes.reflect._

    val implicitClause = '{
      def clause(implicit num: Int) = ???
    }
    val expectedImplicitClause =
      """|{
         |  def clause(using num: scala.Int): scala.Nothing = scala.Predef.???
         |  ()
         |}""".stripMargin

    assert(implicitClause.show == expectedImplicitClause)

    // test ImplicitClauseInCaseClass
    assert(TypeRepr.of[I].typeSymbol.primaryConstructor.paramSymss(1)(0).flags.is(Flags.Given))

    '{()}
