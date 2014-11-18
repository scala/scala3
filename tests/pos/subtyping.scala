class A {
  def test1(): Unit = {
    implicitly[this.type <:< this.type]
    implicitly[this.type <:< A]
  }
}
object test {

  class B
  class C

  def tag1[T](x: T): String & T = ???
  def tag2[T](x: T): T & String = ???

  val x1: Int & String = tag1(0)
  val x2: Int & String = tag2(0)
  val x3: String & Int = tag1(0)
  val x4: String & Int = tag2(0)

}


