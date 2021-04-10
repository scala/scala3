trait Poly {
  def f[T]: String
}

val p: Poly { def f[T]: "hello" } = ???
val q: "hello" = p.f[Int]

trait Box {
  val x: String
}

val b: Box { val x: "goodbye" } = ???
val c: "goodbye" = b.x
