// NOTE: this file is compiled under -source 3.2-migration (i.e. it will rewrite to `: @unchecked`)
// see refutable-pattern-bindings.scala for the version compiled under -source 3.8-migration (which rewrites to `.runtimeChecked`)
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

val (_: Int | _: AnyRef) = ??? : AnyRef
