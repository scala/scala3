object Test {

trait Tc[F[_]]
type OfString[G[_]] = G[String]

def foo[A[_]](fs: A[String])(implicit f: Tc[A]): A[String] = ???
def bar[B[_]](fs: OfString[B]): B[String] = foo(fs)(new Tc[B] {})

}