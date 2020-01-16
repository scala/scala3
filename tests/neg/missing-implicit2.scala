trait X
trait Y
object test:
  def f(given x: X) = ???
  object instances {
    given y: Y = ???
  }
  locally {
    given xFromY(given y: Y): X = ???
    f(given xFromY) // error
  }
  locally {
    object instances2 {
      given xFromY(given Y): X = ???
    }
    f // error
  }
