
trait Animal
class Dog extends Animal

trait Ev[T]

given Ev[Dog] = ???
given Ev[Animal] = ???
given[T: Ev]: Ev[Set[T]] = ???

def f[T: Ev](v: T): Unit = ???

def main =
	val s = Set(new Dog)
//	f(s) // WORKS
	f(Set(new Dog)) // FAILS
	// instantiates to
