trait X
trait Y
object test:
  def f(given x: X) = ???
  object instances {
    given y as Y = ???
  }
  locally {
    given xFromY(given y: Y) as X = ???
    f(given xFromY) // error
  }
  locally {
    object instances2 {
      given xFromY(given Y) as X = ???
    }
    f // error
  }
