import language.experimental.captureChecking

class Source[+T, cap Cap]

def completed[T, cap Cap](result: T): Source[T, {Cap}] =
  //val fut = new Source[T, Cap]()
  val fut2 = new Source[T, {Cap}]()
  fut2: Source[T, {Cap}]






