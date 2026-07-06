import language.experimental.magic

class Date
object Date extends Date

object Foo {

  def foo =
    '''spec
    Parse date string into `Date` structure
    '''
    ???

  val x = 3

  def bar =
    '''spec
    Parse date string into `Date` structure.
    Also: `1 + 2 == 3`, $x
    '''
    ???

  def baz =
    '''spec
    Parse date string into `Date 0` structure
    '''
}