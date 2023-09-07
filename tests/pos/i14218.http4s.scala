// A minimisation from http4s,
// which broke while implementing the fix for i14218.

final class Bar[+F[_]]
object Bar:
  def empty[F[_]]: Bar[F] = new Bar[Nothing]

final class Foo[+F[_]]

object Foo:
  def apply[F[_]](bar: Bar[F] = Bar.empty): Foo[F]    = new Foo

class Test:
  def test[F[_]]: Foo[F] = Foo[F]()

//-- [E007] Type Mismatch Error
//12 |  def test[F[_]]: Foo[F] = Foo[F]()
//   |                           ^^^^^^
//   |              Found:    Bar[[_] =>> Any]
//   |              Required: Bar[F]
//   |
//   |              where:    F is a type in method t1 with bounds <: [_] =>> Any
