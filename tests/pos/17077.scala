class MyProduct extends Product:
    def foo = ???
    override def productArity: Int = 1
    override def productElement(n: Int): Any = 42
    override def canEqual(that: Any): Boolean = that.isInstanceOf[MyProduct]
    def _1 = 42

object MyProductUnapply:
    def unapply(x: Int): MyProduct = MyProduct()

@main def test =
  val v: String | Int = "Blop"
  val res =
    v match
      case MyProductUnapply(y) => y // works: a product of arity 1 is accepted as the return type of unapply
                                    // see UnapplyInvalidReturnType in messages.scala
                                    // and https://docs.scala-lang.org/scala3/reference/changed-features/pattern-matching.html#fixed-arity-extractors
      case _ => 42
  println(res)

