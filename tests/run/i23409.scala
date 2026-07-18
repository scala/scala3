
// dropForMap should be aware of conversions to receiver

import language.implicitConversions

trait Func[F[_]]:
  def map[A, B](fa: F[A])(f: A => B): F[B]

object Func:
  trait Ops[F[_], A]:
    type T <: Func[F]
    def t: T
    def fa: F[A]
    def map[B](f: A => B): F[B] = t.map[A, B](fa)(f)

  object OldStyle:
    implicit def cv[F[_], A](fa0: F[A])(using Func[F]): Ops[F, A] { type T = Func[F] } =
      new Ops[F, A]:
        type T = Func[F]
        def t: T = summon[Func[F]]
        def fa = fa0

  object NewStyle:
    given [F[_], A] => Func[F] => Conversion[F[A], Ops[F, A] { type T = Func[F] }]:
      def apply(fa0: F[A]): Ops[F, A] { type T = Func[F] } =
        new Ops[F, A]:
          type T = Func[F]
          def t: T = summon[Func[F]]
          def fa = fa0
end Func

def works =
  for i <- List(42) yield i

class C[A]
object C:
  given Func[C]:
    def map[A, B](fa: C[A])(f: A => B): C[B] = ??? // must be elided

def implicitlyConverted() = println:
  import Func.OldStyle.given
  //C().map(x => x) --> C()
  for x <- C() yield x

def usingConversion() = println:
  import Func.NewStyle.given
  //C().map(x => x) --> C()
  for x <- C() yield x

@main def Test =
  implicitlyConverted()
  usingConversion()
