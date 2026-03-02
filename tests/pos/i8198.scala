trait Eq[A] {
  extension (x: A)
    def === (y: A): Boolean
    def /== (y: A): Boolean = !(x === y)
}

case class Id[T](id: T)

given idEq: [A] => (eqA: Eq[A]) => Eq[Id[A]] = new {
  extension (i1: Id[A]) def === (i2: Id[A]) = !(i1.id /== i2.id)
}
