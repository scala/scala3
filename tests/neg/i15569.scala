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

@main def Test3 = (None: Option[Foo[?]]) match {
  case _: Option[Foo[t]] =>
    val unsound: Nothing = (5 : Any) : t  // error
    (unsound : Unit => Unit).apply(())
}

@main def Test4 =
  type t >: Any <: Nothing
  val unsound: Nothing = (5 : Any) : t  // error
  (unsound : Unit => Unit).apply(())

@main def Test5 =
  type t >: Any <: Nothing
  val unsound: List[Nothing] = List(5 : Any) : List[t]  // error
  (unsound.head : Unit => Unit).apply(())
