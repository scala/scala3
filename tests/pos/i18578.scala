
trait Animal
class Dog extends Animal

trait Ev[T]

given Ev[Dog] = ???
given Ev[Animal] = ???
given[T: Ev]: Ev[Set[T]] = ???

def f[T: Ev](v: T): Unit = ???

def main =
	val s = Set(new Dog)
	f(s) // Ok
	f(Set(new Dog)) // Error before changes: Ambiguous given instances: both given instance given_Ev_Dog and given instance given_Ev_Animal match type Ev[T]

