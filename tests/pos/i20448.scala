trait Generic[T] {
  type Repr
}

object Generic {
  type Aux[T, R] = Generic[T] { type Repr = R }

  inline given [T <: Product](
    using m: scala.deriving.Mirror.ProductOf[T]
  ): Generic.Aux[T, m.MirroredElemTypes] = ???
}

trait Delta[In] {
  type Out
}

object Delta {
  type Aux[In, Out0] = Delta[In] { type Out = Out0 }

  given [T](using deltaT: Delta[T]): Delta.Aux[Option[T], deltaT.Out] = ???

  given [H, T <: Tuple, HO](using deltaH: => Delta.Aux[H, HO]): Delta.Aux[H *: T, H] = ???

  given [F, G, O]( using gen: Generic.Aux[F, G], genDelta: Delta.Aux[G, O]): Delta.Aux[F, O] = ???
}

object Test {
  case class Bar(of: Option[Bar])

  summon[Delta[Bar]]
}
