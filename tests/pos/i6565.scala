class Err

type Lifted[A] = Err | A

def point[O](o: O): Lifted[O] = o
def [O, U](o: Lifted[O]).map(f: O => U): Lifted[U] = ???
def [O, U](o: Lifted[O]).flatMap(f: O => Lifted[U]): Lifted[U] = ???

val error: Err = Err()

lazy val ok: Lifted[String] = { // ok despite map returning a union
  point("a").map(_ => if true then "foo" else error) // ok
}

lazy val nowAlsoOK: Lifted[String] = {
  point("a").flatMap(_ => point("b").map(_ => if true then "foo" else error))
}
