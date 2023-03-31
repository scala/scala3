// A minimisation of a http4s test failure
// that came while implementing GadtExpr
// which was caused by a change in the selection of which type parameter
// to keep during constraint parameter replacement
// and was fixed by reverting the change
// and instead tweaking the order in which gadt bounds are added during unpickling
class Cat[F[_]]
object Cat:
  given [G[_]]: Cat[G] = new Cat[G]

class Dog

trait Foo[F[_]]:
  def bar[G[x] >: F[x]](using cat: Cat[G], dog: Dog = new Dog) = ()

class Test:
  def meth[F[_]](foo: Foo[F]) = foo.bar
