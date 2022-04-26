val xs: List[Any] = ???

val hd :: tl = xs match
  case Nil => null :: xs
  case _   => xs

val h :: t = xs

val a :: b =
  if xs.isEmpty then null :: xs
  else xs

val c :: d =
  try xs.head :: xs
  catch case _: NoSuchElementException => null :: xs

val e :: f =
  val zero = null :: Nil
  if xs.isEmpty then zero
  else xs

val j :: k =
  for
    (x: String) <- xs
  yield x
