import scala.deriving._

trait FunctorK[F[_[_]]]
object FunctorK {
  given [C]: FunctorK[[F[_]] =>> C] with {}
  given [T]: FunctorK[[F[_]] =>> Tuple1[F[T]]] with {}

  def derived[F[_[_]]](using m: Mirror { type MirroredType[X[_]] = F[X] ; type MirroredElemTypes[_[_]] }, r: FunctorK[m.MirroredElemTypes]): FunctorK[F] = new FunctorK[F] {}
}

