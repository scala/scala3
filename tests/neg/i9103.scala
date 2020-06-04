trait Show[T] {def show(a: T): String}

object S extends LowPriorityInstances {
  class Permissions
}

sealed trait LowPriorityInstances
object LowPriorityInstances {
  given S.Permissions = ???
  given Show[S.Permissions] = _ => "perms"
}

@main def test =
  println(implicitly[S.Permissions])       // error
  println(implicitly[Show[S.Permissions]]) // error

