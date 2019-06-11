class Box[F[_]]

class C[X]
class D[X] extends C[String]

object Test {
  def f[F[_]](x: Box[F]) = ???
  def db: Box[D] = ???
  def cb: Box[C] = db  // error
  f[[X] =>> C[X]](db)   // error
}
