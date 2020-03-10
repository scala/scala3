import scala.quoted._

class Test(using qctx: QuoteContext) {
  def step(k: (String => Expr[Unit])): Expr[Unit] = '{}
  def meth(): Unit = '{
    (i: Int) => ${ step(el => '{} ) }
  }
}
