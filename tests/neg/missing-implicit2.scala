trait X
trait Y
object test:
  def f(using x: X) = ???
  object instances {
    given Y as y = ???
  }
  locally {
    given xFromY(using y: Y) as X = ???
    f(using xFromY) // error
  }
  locally {
    object instances2 {
      given xFromY(using Y) as X = ???
    }
    f // error
  }
