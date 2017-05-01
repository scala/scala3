case class OldTuple2[+T1, +T2](_1: T1, _2: T2) extends Product2[T1, T2]

class Foo[t[x]<: OldTuple2[Int, x]]

class MyPair[z](a: Int, b: z) extends OldTuple2[Int, z](a,b)

object foo extends Foo[MyPair]


trait Monad[m[x <: Bound[x]], Bound[x], a] // TODO: variances!
trait ListMonad[a] extends Monad[List, [X] => Any, a] // Dotty difference: Any is not a legal argument for hk type.

trait MyOrdered[a]
trait MySet[x <: MyOrdered[x]]
trait SetMonad[a <: MyOrdered[a]] extends Monad[MySet, MyOrdered, a]
