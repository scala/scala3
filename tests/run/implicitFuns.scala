object Test {
  def main(args: Array[String]) = {

    implicit val world: String = "world!"

    val i1 = (implicit (s: String) => s.length > 2)
    val i2 = {implicit (s: String) => s.length > 2}

    assert(i1)
    assert(i2)

    val x: implicit String => Boolean = { implicit (s: String) => s.length > 2 }

    val xx: implicit (String, Int) => Int = implicit (x: String, y: Int) => x.length + y

    val y: String => Boolean = x

    object nested {
      implicit val empty: String = ""
      assert(!x)
    }

    val yy: (String, Int) => Any = xx

    val z1: implicit String => Boolean = implicitly[String].length >= 2
    assert(z1)

    type StringlyBool = implicit String => Boolean

    val z2: StringlyBool = implicitly[String].length >= 2
    assert(z2)

    type Stringly[T] = implicit String => T

    val z3: Stringly[Boolean] = implicitly[String].length >= 2
    assert(z3)

    type GenericImplicit[X] = implicit X => Boolean

    val z4: GenericImplicit[String] = implicitly[String].length >= 2
    assert(z4)

    val b = x("hello")

    val b1: Boolean = b

    val bi = x

    val bi1: Boolean = bi

    val c = xx("hh", 22)

    val c1: Int = c

    Contextual.main(args)
  }
}

object Contextual {

  class Key[+V]

  class Context(bindings: Map[Key[Any], Any]) {
    def binding[V](key: Key[V]): Option[V] =
      bindings.get(key).asInstanceOf[Option[V]]
    def withBinding[V](key: Key[V], value: V): Context =
      new Context(bindings + ((key, value)))
  }

  val rootContext = new Context(Map())

  val Source = new Key[String]
  val Options = new Key[List[String]]

  type Ctx[T] = implicit Context => T

  def ctx: Ctx[Context] = implicitly[Context]

  def compile(s: String): Ctx[Boolean] =
    runOn(new java.io.File(s))(ctx.withBinding(Source, s)) >= 0

  def runOn(f: java.io.File): Ctx[Int] = {
    val options = List("-verbose", "-explaintypes")
    process(f)(ctx.withBinding(Options, options))
  }

  def process(f: java.io.File): Ctx[Int] =
    ctx.binding(Source).get.length - ctx.binding(Options).get.length

  def main(args: Array[String]) = {
    implicit val context: Context = rootContext
    assert(compile("abc"))
    assert(compile("ab"))
    assert(!compile("a"))
  }
}
