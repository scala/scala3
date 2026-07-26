import language.experimental.magic

class Date
object Date extends Date

object Foo {

  def foo =
    '''spec
    Parse date string into `Date` structure
    '''
    ???

  def foo2 =
    '''spec
    Parse date string ${1}
    into `Date` structure
    '''
    ???

  val x = 3

  def bar =
    '''spec
      ```
      println("hello")
      println(world)
      ```
      ${1}
    '''
    ???

  def bar2 =
    '''spec
    Parse date string into `Date` structure.
    Also: `1 + 2 == 3`,
    Also:
      ```
      println("hello")
      println("world")
      ```
    '''
    ???

  def baz =
    '''spec
    Parse date string into `Date 0` structure
    '''
    ???

  def bam =
    '''spec
    This is a `List[Int]`
    '''
}