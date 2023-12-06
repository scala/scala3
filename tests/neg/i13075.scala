object Implementing_Tuples:

  sealed trait Tup
  case class ConsTup[T, H <: Tup](head: T, tail: H) extends Tup
  case object EmptyTup extends Tup

  val *: = ConsTup                     // for unapply
  type *:[H, T <: Tup] = ConsTup[H, T] // for type matching
  type EmptyTup = EmptyTup.type        // for type matching

  extension [H](head: H)
    def *:[T <: Tup](tail: T) = ConsTup(head, tail)

  type Fold[T <: Tup, Seed, F[_,_]] = T match
    case EmptyTup => Seed
    case h *: t => Fold[t, F[Seed, h], F]

  extension [T <: Tup](v: T)
    def fold[Seed, F[_,_]](seed: Seed)(
      fn: [C, Acc] => (C, Acc) => F[C, Acc]
    ): Fold[T, Seed, F] =
      (v match
        case EmptyTup => seed
        case h *: t => t.fold(fn(h, seed))(fn)
      ).asInstanceOf[Fold[T, Seed, F]]

  extension [T <: Tup](v: T) def reversed: Tup =
    v.fold[EmptyTup, [C, Acc] =>> Acc match {
      case h *: t => C *: h *: t
    }](EmptyTup)(
      [C, Acc] => (c: C, acc: Acc) => acc match
        case _@(_ *: _) => c *: acc  // error
    )

  @main def testProperFold =
    val t = (1 *: '2' *: "foo" *: EmptyTup)
    val reversed: (String *: Char *: Int *: EmptyTup) = t.reversed // error
    println(reversed)

end Implementing_Tuples