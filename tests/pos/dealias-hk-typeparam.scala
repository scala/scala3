type Result[F[_], Err, Res] = F[Either[Err, Res]]

trait TypeClass[F[_]]

def id[F[_]: TypeClass, A](x: F[A]) = x

def test[F[_]: TypeClass](x: Result[F, String, Int]) =
  id(x)