class Foo {
  def fun: Int => Int = n => n + x.size   // error: itself ok, but fun is called below
  fun(5)                                          // error: select on raw value

  def getThis: Foo = this
  getThis.x                                       // error

  def getThis(x: Int): Foo = this
  getThis(56).x                                   // error

  val x = "hello"
}