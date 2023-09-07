// Minimisation of how the fix for t9419 affected specs2
class MustExpectable[T](tm: () => T):
  def must_==(other: => Any) = tm() == other

class Foo

object Main:
  implicit def theValue[T](t: => T): MustExpectable[T] = new MustExpectable(() => t)
  def main(args: Array[String]): Unit =
    val cls = classOf[Foo]
    val instance = new Foo()
    val works = cls must_== cls
    val fails = instance.getClass must_== cls
