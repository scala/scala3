object a:
  trait Foo[T]
  given Foo[Unit] = ???

val b = a

def test1 = summon[b.Foo[Unit]]  // no ambiguity between b.given_Foo and a.given_Foo

val n: Long = 1
val total: BigInt = 2
val remainder = n % identity(total)  // object BigInt is in implicit scope of `total.type`

trait Show[T] {def show(a: T): String}

object S extends LowPriorityInstances {
  class Permissions
}

sealed trait LowPriorityInstances
object LowPriorityInstances {
  given S.Permissions = ???
  given Show[S.Permissions] = _ => "perms"
}

def test =
  println(implicitly[S.Permissions])      // companions of base classes of S are in implicit scope
  println(implicitly[Show[S.Permissions]])

