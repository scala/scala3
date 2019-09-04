import scala.quoted._

object Test {
  given as QuoteContext = ???
  def step(k: (String => Expr[Unit])): Expr[Unit] = '{}
  def meth(): Unit = '{
    (i: Int) => ${ step(el => '{} ) }
  }
}
