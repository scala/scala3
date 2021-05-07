opaque type ProductK[F[_], T <: Tuple] = Tuple.Map[T, F]
object ProductK:
  def of[F[_], T <: Tuple](t: Tuple.Map[T, F]): ProductK[F, T] = t
