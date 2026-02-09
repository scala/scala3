import language.experimental.captureChecking

class Source[+T, Cap^]

def completed[T, Cap^](result: T): Source[T, {Cap}] =
  //val fut = new Source[T, Cap]()
  val fut2 = new Source[T, {Cap}]()
  fut2: Source[T, {Cap}]






