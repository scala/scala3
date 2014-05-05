trait Cloning {
  trait Foo
  def fn(g: Any => Unit): Foo

  class Star { def *(a: Cloning.this.Foo): Cloning.this.Foo }

  implicit def mkStar(i: Int): Star = new Star { def *(a: Foo): Foo = null }

  val pool = 4 * fn { case ghostSYMBOL: Int => ghostSYMBOL * 2 }
}
