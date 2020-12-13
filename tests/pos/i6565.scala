class Err

type Lifted[A] = Err | A

def point[O](o: O): Lifted[O] = o
extension [O, U](o: Lifted[O]) def map(f: O => U): Lifted[U] = ???
extension [O, U](o: Lifted[O]) def flatMap(f: O => Lifted[U]): Lifted[U] = ???

val error: Err = Err()

lazy val ok: Lifted[String] = {
  point("a").flatMap(_ => if true then "foo" else error)
}

lazy val nowAlsoOK: Lifted[String] = {
  point("a").flatMap(_ => point("b").flatMap(_ => if true then "foo" else error))
}
