import scala.quoted.{_, given}

trait IsExpr {
  type Underlying
}

val foo: IsExpr = ???

def g()(given QuoteContext): Unit = {
  val a = '[foo.Underlying]
  ()
}
