trait X
trait Y
object test:
  def f(using x: X) = ???
  object instances {
    given y: Y = ???
  }
  locally {
    given xFromY(using y: Y): X = ???
    f(using xFromY) // error
  }
  locally {
    object instances2 {
      given xFromY(using Y): X = ???
    }
    f // error
  }
