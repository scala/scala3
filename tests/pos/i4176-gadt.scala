object i4176 {
  sealed trait TNat
  case class TZero() extends TNat
  case class TSucc[N <: TNat]() extends TNat

  object TNatSum {
    sealed trait TSum[M, N, R]
    case class TSumZero[N]() extends TSum[TZero, N, N]
    case class TSumM[M <: TNat, N, R <: TNat](sum: TSum[M, N, R]) extends TSum[TSucc[M], N, TSucc[R]]
  }
  import TNatSum.*

  implicit def tSumZero[N]: TSum[TZero, N, N] =
    TSumZero()
  implicit def tSumM[M <: TNat, N, R <: TNat](implicit sum: TSum[M, N, R]): TSum[TSucc[M], N, TSucc[R]] =
    TSumM(sum)

  sealed trait Vec[T, N <: TNat]
  case object VNil extends Vec[Nothing, TZero] // fails but in refchecks
  case class VCons[T, N <: TNat](x: T, xs: Vec[T, N]) extends Vec[T, TSucc[N]]

  def append0[T, M <: TNat, N <: TNat, R <: TNat]($this: Vec[T, M], that: Vec[T, N])(implicit tsum: TSum[M, N, R]): Vec[T, R] =
    ($this, tsum) match {
      case (VNil, TSumZero()) => that
      case (VCons(x, xs), TSumM(sum)) => VCons(x, append0(xs, that)(sum))
    }

  def append[T, M <: TNat, N <: TNat, R <: TNat]($this: Vec[T, M], that: Vec[T, N])(implicit tsum: TSum[M, N, R]): Vec[T, R] =
      tsum match {
        case TSumZero() =>
          $this match { case VNil => that }
        case TSumM(sum) =>
          $this match { case VCons(x, xs) => VCons(x, append(xs, that)(sum)) }
      }

}
