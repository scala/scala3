import scala.quoted._

trait IsExpr {
  type Underlying
}

val foo: IsExpr = ???

def g()(using Quotes): Unit = {
  val a = Type.of[foo.Underlying]
  ()
}
