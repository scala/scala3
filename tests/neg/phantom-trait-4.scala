
class Foo {

  object Boo1 extends Phantom // error

  def foo = {
    object Boo2 extends Phantom // error
    42
  }
}

object Foo {
  object Boo1 extends Phantom

  def foo = {
    object Boo2 extends Phantom // error
    42
  }
}

package foo {
  object Boo1 extends Phantom
}
