class Computes[T]

case class Result[T](val computes : Computes[T])

def impl[T](computes : Computes[T]) : Result[T] = {
  val result =
    if ??? then {
      impl(??? : Computes[_])
    } else {
      Result(computes)
    }
  result match {
    case Result(r) => {
      Result(r) // error
    }
  }
}
