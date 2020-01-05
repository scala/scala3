trait X
trait Y
object test with
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
      given xFromY: Y => X = ???
    }
    f // error
  }
