trait Cloning {
  trait Foo
  def fn(g: Any => Unit): Foo

  implicit def mkStar(i: Int): AnyRef{def *(a: Cloning.this.Foo): Cloning.this.Foo} = new { def *(a: Foo): Foo = null }

  val pool = 4 * fn { case ghostSYMBOL: Int => ghostSYMBOL * 2 }
}
