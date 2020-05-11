import scala.quoted._

class Test {
  def step(using s: Scope)(k: (String => s.Expr[Unit])): s.Expr[Unit] = '{}
  def meth(using s: Scope)(): Unit = '{
    (i: Int) => ${ step(el => '{} ) }
  }
}
