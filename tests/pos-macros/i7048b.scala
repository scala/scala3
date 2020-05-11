import scala.quoted._

trait IsExpr {
  type Underlying
}

val foo: IsExpr = ???

def g(using s: Scope)(): Unit = {
  val a = '[foo.Underlying]
  ()
}
