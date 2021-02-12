import scala.quoted.*

case class Vec[Idx, T](size: Idx, get: Idx => T) {
  def apply(idx: Idx): T = get(idx)

  def vecMap[U](f: T => U): Vec[Idx, U] = Vec(size, i => f(get(i)))

  def zipWith[U, V](vec2: Vec[Idx, U], f: (T, U) => V): Vec[Idx, V] =
    Vec(size, i => f(get(i), vec2(i)))
}

case class OVec[Idx, T, Unt](size: Idx, update: (Idx, T) => Unt) {
  def vecAssign(vecIn: Vec[Idx, T]): Vec[Idx, Unt] =
    Vec(vecIn.size, i => update(i, vecIn(i)))
}

object Vec {
  def fromArray[T](a: Array[T]): (Vec[Int, T], OVec[Int, T, Unit]) =
    (Vec(a.size, i => a(i)), OVec(a.size, (i, v) => a(i) = v))
}
