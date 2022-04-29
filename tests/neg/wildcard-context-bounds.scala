import language.experimental.wildcardContextBounds

trait Showable[A] {
  extension(a: A) def show: String
}
object Showable

given Showable[String] with
  extension(a: String) def show: String = a

given Showable[Int] with
  extension(a: Int) def show: String = a.toString

def returns: ? : Showable = ??? // error

def ascription: Unit = {
  val x: ? : Showable = ??? // error
}

def lambda: Unit = {
  (x: ? : Showable) => x.show // error // error
}

def ascribedLambda1: Unit = {
  (x => x + 1): (Int => ? : Showable) // error
}

def ascribedLambda2: Unit = {
  ((x: Int) => x + 1): (? : Showable => Showable) // error // error
}

trait Foo extends Seq[? : Showable] // error

type Alias[? : Showable] = Int // error // error
type OtherAlias = ? : Showable // error
