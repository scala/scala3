class C[F[X]]
class D[F[+X]]

type Id[X] = X

def test =
  val x = C[Id]()
  val y = D[Id]()

object Test2 {
  trait S {
    type F[+X]
  }
  trait T {
    type F[-X]
  }
  object O extends S, T {
    type F[X] = Int
  }
}
