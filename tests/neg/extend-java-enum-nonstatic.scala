import java.{lang => jl}

object TestSuite:
  def test(op: => Unit): Unit = op
  test {
    enum E extends jl.Enum[E] { case A } // error: enum extending java.lang.Enum must be declared in a static scope
  }

class Container:
  enum E extends jl.Enum[E] { case A } // error: enum extending java.lang.Enum must be declared in a static scope

object Wrap:
  def force =
    enum E extends jl.Enum[E] { case A } // error: enum extending java.lang.Enum must be declared in a static scope

trait Universe:
  enum E extends jl.Enum[E] { case A } // error: enum extending java.lang.Enum must be declared in a static scope

object Static:
  enum E extends jl.Enum[E] { case A } // ok
