
sealed trait Foo {

  type X
  type Y <: X

  def x: X

  def f(y: Y) = println("ok")

  given Typeable[X, Y] = new Typeable {
    def unapply(x: X): Option[Y] = Some(x.asInstanceOf[Y])
  }

  object Z {
    def unapply(arg: Y): Option[Int] = Some(9)
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    test(new Foo { type X = Int; type Y = Int; def x: X = 1 })
  }

  def test(foo: Foo): Unit = {
    import foo.given
    foo.x match {
      case x @ foo.Z(i) => // `x` is refined to type `foo.Y`
        foo.f(x)
        println(i)
    }
  }
}
