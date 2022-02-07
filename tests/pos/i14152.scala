val a1 = {
  object O1 extends AnyRef
  Array(O1)
}
val a2: Array[_ <: AnyRef] = aa1

val aa1 = {
  object O1 extends AnyRef
  Array(Array(O1))
}
val aa2: Array[_ <: Array[_ <: AnyRef]] = aa1

val aaa1 = {
  object O1 extends AnyRef
  Array(Array(Array(O1)))
}
val aaa2: Array[_ <: Array[_ <: Array[_ <: AnyRef]]] = aaa1


// Let's make sure avoidance still does the right thing given abstract type constructors

class Inv[T](x: T)

def foo[F[_]](fn: [A] => Inv[A] => F[A]) =
  object O1 extends AnyRef
  val res0 = fn(new Inv(fn(new Inv[O1.type](O1))))
  val res1: F[F[O1.type]] = res0
  res1 // checked with -Xprint:typer that this widens to Any
       // instead of the original F[F[O1.type]]
       // or the incorrectly avoided F[? <: F[? <: Object]]
