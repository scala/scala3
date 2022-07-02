trait Foo[X >: Any <: Nothing]

def andThenSub[A, B, C](f: A <:< B, g: B <:< C): A <:< C =
  f.andThen(g)

@main def Test = (None: Option[Foo[?]]) match {
  case _: Option[Foo[t]] =>
    val unsound: Any <:< Nothing = andThenSub[Any, t, Nothing](summon, summon) // error
    unsound("hi :)")
}
@main def Test2 =
  type t >: Any <: Nothing
  val unsound: Any <:< Nothing = andThenSub[Any, t, Nothing](summon, summon) // error
  unsound("hi :)")

