class Err

type Lifted[A] = Err | A

def point[O](o: O): Lifted[O] = o
def (o: Lifted[O]) map [O, U] (f: O => U): Lifted[U] = ???
def (o: Lifted[O]) flatMap [O, U] (f: O => Lifted[U]): Lifted[U] = ???

val error: Err = Err()

lazy val ok: Lifted[String] = { // ok despite map returning a union
  point("a").map(_ => if true then "foo" else error) // ok
}

lazy val bad: Lifted[String] = { // found Lifted[Object]
  point("a").flatMap(_ => point("b").map(_ => if true then "foo" else error)) // error
}
