final class Foo {
  def getName1(f: () => String)(g: () => String): () => String = () => f() + g()    // error

  val a1: () => String = () => this.name    // error
  val b1: () => String = () => "Jack"
  val f1 = getName1(a1)(b1)

  f1()                                     // error

  def getName2(f: () => String)(g: () => String): () => String = () => f() + g()

  val a2: () => String = () => this.name
  val b2: () => String = () => "Jack"
  val f2 = getName2(a2)(b2)

  val name = "hello"

  f1()                                  // ok, inited
}