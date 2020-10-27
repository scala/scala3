import language.experimental.namedTypeArguments
class C[X, Y] { def apply[Z](x: X, y: Y, z: Z) = (x, y, z) }

object Test {
  def f[X, Y, Z]: C[X, Y] = new C[X, Y]
  f[Int, Boolean, Any][String](1, true, "") // OK
  f[X = Int](1, true, "") // OK, Y and Z are inferred
  f[Z = Any, X = Int](1, true, "") // OK
}