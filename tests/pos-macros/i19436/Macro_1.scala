
import scala.quoted.*
import scala.compiletime.summonInline

trait SomeImplicits:
  given int: Int

object Macro:

  transparent inline def testSummon: SomeImplicits => Int = ${ testSummonImpl }

  private def testSummonImpl(using Quotes): Expr[SomeImplicits => Int] =
    import quotes.reflect.*
    '{
      (x: SomeImplicits) =>
        import x.given
        summonInline[Int]
    }