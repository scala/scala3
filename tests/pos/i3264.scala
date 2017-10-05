trait Foo[+F[_]] {
  def bar: Bar[F]
}

trait Bar[+F[_]]

trait Base[+A]
trait Sub[A] extends Base[A]

class Test {
  def makeFoo(barSub: Bar[Sub]): Foo[Base] =
    new Foo[Base] {
      def bar/*: Bar[Base]*/ = barSub
    }
}
