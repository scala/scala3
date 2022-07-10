trait Foo[X >: Any <: Nothing]

def andThenSub[A, B, C](f: A <:< B, g: B <:< C): A <:< C =
  f.andThen(g)

@main def Test = (None: Option[Foo[?]]) match {
  case _: Option[Foo[t]] => // error
    val unsound: Any <:< Nothing = andThenSub[Any, t, Nothing](summon, summon)
    unsound("hi :)")
}
@main def Test2 =
  type t >: Any <: Nothing // error
  val unsound: Any <:< Nothing = andThenSub[Any, t, Nothing](summon, summon)
  unsound("hi :)")

@main def Test3 = (None: Option[Foo[?]]) match {
  case _: Option[Foo[t]] =>  // error
    val unsound: Nothing = (5 : Any) : t
    (unsound : Unit => Unit).apply(())
}

@main def Test3ok = (None: Option[Foo[?]]) match {
  case _: Option[Foo[_]] =>  // ok
}

@main def Test4 =
  type t >: Any <: Nothing  // error
  val unsound: Nothing = (5 : Any) : t
  (unsound : Unit => Unit).apply(())

@main def Test5 =
  type t >: Any <: Nothing // error
  val unsound: List[Nothing] = List(5 : Any) : List[t]
  (unsound.head : Unit => Unit).apply(())

@main def Test6 =
  type t[X] >: Any <: Nothing  // error
  val unsound: List[Nothing] = List(5 : Any) : List[t[String]]
  (unsound.head : Unit => Unit).apply(())

@main def Test7 =
  trait A:
    type t >: Any <: Nothing
  val unsound: List[Nothing] = List(5 : Any) : List[A#t] // error
  (unsound.head : Unit => Unit).apply(())
