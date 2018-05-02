final class Foo {
  def getName1(foo: Foo): String = foo.name                        // error
  getName1(this)                                                   // error


  def getName2(foo: () => String): String = foo()                  // error
  getName2(() => this.name)                                        // error

  def getName2b(foo: () => String): String = "hello"
  getName2b(() => this.name)

  def getName3(foo: () => String): () => Int = () => foo().size
  val sizeFun = getName3(() => this.name)

  def getName4(foo: () => String): () => Int = () => foo().size // error
  val fun4 = getName4(() => this.name)                          // error
  fun4()                                                        // error

  def getName5(foo: () => String): () => () => Int = () => () => foo().size
  val fun5a = getName5(() => this.name)
  val fun5b = fun5a()

  def getName6(foo: () => String): () => () => Int = () => () => foo().size  // error
  val fun6a = getName6(() => this.name)                                      // error
  val fun6b = fun6a()
  fun6b()                                                                    // error

  val name = "hello"
}