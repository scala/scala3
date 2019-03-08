// spurious dead-code
class Foo {
  def bar(x: Either[String, Base]): Unit = {
    x match {
      case Left(s) => println("l")
      case Right(Base(s)) => println("r")
    }
  }

  def bar(x: Base): Unit = x match {
    case Zero =>
  }

  def anonymous: Base = new Base("bla") {}
}

sealed abstract case class Base(value: String) {
  override def toString = value // ok?
}

object Zero extends Base("zero")
