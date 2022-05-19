// scalac: -Werror
enum Foo:
  case One(value: String)
  case Two(value: Long, month: java.time.Month)

object Issue:
  def doSomething(foo: Foo): String = foo match
    case Foo.One(x)    => s"1 $x"
    case Foo.Two(x, y) => s"2 $x $y"
