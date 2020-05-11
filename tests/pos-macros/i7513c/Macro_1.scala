import scala.quoted._

object Macros {
  trait Quoted {
    def foo: Int
  }
  inline def quote: Quoted = ${ quoteImpl }

  def quoteImpl(using s: Scope): s.Expr[Quoted] = '{
    new Quoted {
      def foo = ???
    }
  }
}
