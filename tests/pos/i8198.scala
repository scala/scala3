trait Eq[A] {
  def (x: A) === (y: A): Boolean
  def (x: A) /== (y: A): Boolean = !(x === y)
}

case class Id[T](id: T)

given idEq[A](using eqA: Eq[A]) as Eq[Id[A]] = new {
  def (i1: Id[A]) === (i2: Id[A]) = !(i1.id /== i2.id)
}
