enum Result[+T, +E]:
  case Ok[+T](value: T) extends Result[T, Nothing]
  case Err[+E](error: E) extends Result[Nothing, E]

def catchException(body: => Int): Result[Int, Null] =
  try Result.Ok(body)
  catch case _: Throwable => Result.Err(null)