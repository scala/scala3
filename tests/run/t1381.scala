object Test {
  def main(args: Array[String]): Unit = {
    Test1.test()
    Test2.test()
    Test3.test()
  }
}

object Test1 {
  class Bar[T](n: Int) {
    println(n)
  }
  implicit def const[T](x: T): Bar[T] = new Bar[T](1)

  def bar[T](e: T): Any = new Bar[T](2)
  def bar[T](e: Bar[T]): Any = new Bar[T](3)

  val b: Bar[Int] = new Bar[Int](4)

  def test(): Unit = {
    bar(b)
    bar(5)
  }
}

object Test2 {
  trait A; trait B
  class C1 {
    def f(x: A): Unit = println("A")
  }
  class C2 extends C1 {
    def f(x: B): Unit = println("B")
  }
  object Test extends C2 with App {
    implicit def a2b(x: A): B = new B {}
    def test(): Unit = {
      f(new A {})
      f(new B {})
    }
  }
  def test(): Unit = Test.test()
}

object Test3 {
  trait A; trait B
  class C extends A with B
  def fr(x: A): A = {
    println("frA")
    x
  }
  def fr(x: B): B = {
    println("frB")
    x
  }
  def test(): Unit = {
    val a: A = fr(new C)
    val b: B = fr(new C)
  }
}
