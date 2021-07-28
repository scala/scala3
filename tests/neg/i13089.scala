package pkg:

  trait Bar

  extension (bar : Bar)
    def fails : Unit = {} // error

  def baz(x: String): Boolean = true // error

package pkg:

  trait Foo
  extension (foo : Foo)
    def fails : Unit = {}
    def works : Unit = {}

  extension (bar : Bar)
    def works : Unit = {}

  def baz(x: Int): Boolean = true

