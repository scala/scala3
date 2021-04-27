
class Tasty {
  type Term
  type Select <: Term

  given scala.reflect.ClassTag[Term] = ???
  given scala.reflect.ClassTag[Select] = ???
  object Select {
    def unapply(x: Select): Boolean = ???
  }
}

object Foo {
  def impl(using tasty: Tasty) : Unit = {
    import tasty.{_, given}
    val Select() = (??? : Term)
  }
}