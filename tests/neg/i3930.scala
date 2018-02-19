object A {
  def a[F](x: Int) = 0
  def a[F](x: String) = 0
}
object Test extends App {
  A.a[String][Int](3) == 1 // error: ambiguous
}
