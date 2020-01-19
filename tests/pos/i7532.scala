
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
  def impl with (tasty: Tasty) : Unit = {
    import tasty.{_, given _}
    val Select() = (??? : Term)
  }
}