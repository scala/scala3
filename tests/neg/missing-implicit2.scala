trait X
trait Y
object test:
  def f(using x: X) = ???
  object instances {
    given Y as y = ???
  }
  locally {
    given (y: Y) => X as xFromY = ???
    f(using xFromY) // error
  }
  locally {
    object instances2 {
      given (Y) => X as xFromY = ???
    }
    f // error
  }
