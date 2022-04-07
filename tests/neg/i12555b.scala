trait Noop[T]:
  //note: making this "not inline" fixes the result
  inline def noop(fa: T): T

object Noop {
  inline def noop[T](alg: T)(using n: Noop[T]): T = n.noop(alg)
}

import Noop.*

final case class User(name: String, age: Int)

inline given Noop[User] = a => a  // error

val u = User("hello", 45)

@main
def run = println(Noop.noop(u))
