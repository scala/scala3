trait Monad[m[x <: Bound[x]], Bound[x], a] // TODO: variances!
trait ListMonad[a] extends Monad[List, [X] =>> Any, a] // Dotty difference: Any is not a legal argument for hk type.

trait MyOrdered[a]
trait MySet[x <: MyOrdered[x]]
trait SetMonad[a <: MyOrdered[a]] extends Monad[MySet, MyOrdered, a]
