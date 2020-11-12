import scala.quoted._

trait IsExpr {
  type Underlying
}

val foo: IsExpr = ???

def g()(using QuoteContext): Unit = {
  val a = Type.of[foo.Underlying]
  ()
}
