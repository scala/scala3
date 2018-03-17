class Apply { def apply[A](x: Int) = 1 }
object A {
  def a[F] = new Apply
  def a[F](x: String) = 0
}
object Test extends App {
  assert(A.a[String][Int](3) == 1)
}
