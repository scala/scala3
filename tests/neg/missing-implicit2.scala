trait X
trait Y
object test:
  def f with (x: X) = ???
  object instances {
    given y as Y = ???
  }
  locally {
    given xFromY with (y: Y) as X = ???
    f.with(xFromY) // error
  }
  locally {
    object instances2 {
      given xFromY with Y as X = ???
    }
    f // error
  }
