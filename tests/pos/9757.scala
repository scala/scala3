//> using options -source:3.3

type RemoveFrom[R, A] = R match {
  case A & newType => newType
}

def removeOnePart[R, PartR, A](f: R => A, partR: PartR): RemoveFrom[R, PartR] => A = ???

trait A {}
trait B {}

val f: (A & B) => Int = ???
val f2 = removeOnePart(f, new A {})
