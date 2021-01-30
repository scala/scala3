
import scala.quoted.*

class Blas1[Idx, T, Unt](tring: Ring[T], vec: VecOp[Idx, Unt]) {
  import tring.*
  import vec.*

  implicit class Blas1VecOps(v1: Vec[Idx, T]) {
    def `*.`(v2: Vec[Idx, T]): Vec[Idx, T] = v1.zipWith(v2, mul)
  }

  implicit class Blas1OVecOps(vout: OVec[Idx, T, Unt]) {
    def :=(vin: Vec[Idx, T]): Unt = iter(vout.vecAssign(vin))
  }
  override def toString(): String = s"Blas1($tring, $vec)"
}

class Blas2[Idx, T, Unt](tring: Ring[T], vec: VecROp[Idx, T, Unt]) extends Blas1[Idx, T, Unt](tring, vec) {
  import tring.*
  import vec.*

  implicit class Blas2VecOps(v1: Vec[Idx, T]) {
    def dot(v2: Vec[Idx, T]): T = reduce(add, zero, v1 `*.` v2)
  }

  implicit class Blas2MatOps(a: Vec[Idx, Vec[Idx, T]]) {
    def *(v: Vec[Idx, T]): Vec[Idx, T] = a.vecMap(x => v dot x)
  }
  override def toString(): String = s"Blas2($tring, $vec)"
}
