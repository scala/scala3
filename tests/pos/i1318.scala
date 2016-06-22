object Foo {
  class S(i: Int)
  case class T(i: Int) extends S(i)

  object T {
    def unapply(s: S): Option[(Int, Int)] = Some(5, 6)
    // def unapply(o: Object): Option[(Int, Int, Int)] = Some(5, 6, 7)
  }

  val s = new S(5)

  s match {
    // case T(x, y, z) => println(x + y + z)
    case T(x, y) => println(x + y)
    case T(x) => println(x)
    case _ => println("not match")
  }
}

object Bar {
  case class T(i: Int)
  class S(i: Int) extends T(i)

  object T {
    def unapply(s: S): Option[(Int, Int)] = Some(5, 6)
    // def unapply(o: Object): Option[(Int, Int, Int)] = Some(5, 6, 7)
  }

  val s = new S(5)

  s match {
    // case T(x, y, z) => println(x + y + z)
    case T(x, y) => println(x + y)
    case T(x) => println(x)
    case _ => println("not match")
  }
}

