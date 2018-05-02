final class Foo {
  def getSize(f: () => String): () => Int = () => f().size    // error

  val f1 = getSize(() => this.name)          // error
  val f2 = getSize(() => "Jack")

  f2()
  f1()        // error
  f2()
  f1()        // error

  val name = "hello"

  f1()        // ok
  f2()        // ok
}