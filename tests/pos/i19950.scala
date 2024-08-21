
trait Apply[F[_]]:
  extension [T <: NonEmptyTuple](tuple: T)(using toMap: Tuple.IsMappedBy[F][T])
    def mapN[B](f: Tuple.InverseMap[T, F] => B): F[B] = ???

given Apply[Option] = ???
given Apply[List] = ???
given Apply[util.Try] = ???

@main def Repro = (Option(1), Option(2), Option(3)).mapN(_ + _ + _)