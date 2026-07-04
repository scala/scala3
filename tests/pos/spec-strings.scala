import language.experimental.magic

class Date
object Date extends Date

object Foo {
  def foo =
    '''spec
    Parse date string into `Date` structure
    '''
    ???
  def bar =
    '''spec
    Parse date string into `Date` structure ${"foo"}
    '''
    ???
}