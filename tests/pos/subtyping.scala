class A {
  def test1(): Unit = {
    implicitly[this.type <:< this.type]
    implicitly[this.type <:< A]
  }
}
