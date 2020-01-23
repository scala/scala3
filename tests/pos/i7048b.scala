import scala.quoted._

trait IsExpr {
  type Underlying
}

val foo: IsExpr = ???

def g() with QuoteContext : Unit = {
  val a = '[foo.Underlying]
  ()
}
