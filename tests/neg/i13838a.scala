object TooSlow {
  trait EqSyntax {
    implicit def catsSyntaxEq[A: Eq](a: A): EqOps[A] = ???
  }

  final class EqOps[A]

  object eq extends EqSyntax

  import eq._

  sealed abstract class Foo[A]
  object Foo {
    implicit def eqFoo[A: Eq]: Eq[Foo[A]] = ???
  }

  type FooT[F[_], A] = F[Foo[A]]
  object FooT {
    def liftF[F[_], A](fa: F[A]): F[Foo[A]] =
      map(fa)(???)  // error

    def map[F[_], A, B](ffa: F[Foo[A]])(f: A => B): F[Foo[B]] =
      ???
  }

  trait Order[A] extends Eq[A]

  trait Eq[A]

  object Eq {
    implicit def catsKernelOrderForTuple14[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](implicit A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9], A10: Order[A10], A11: Order[A11], A12: Order[A12], A13: Order[A13]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] = ???
    implicit def catsKernelOrderForTuple13[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](implicit A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9], A10: Order[A10], A11: Order[A11], A12: Order[A12]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] = ???
    implicit def catsKernelOrderForTuple12[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](implicit A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9], A10: Order[A10], A11: Order[A11]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] = ???
    implicit def catsKernelOrderForTuple11[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](implicit A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9], A10: Order[A10]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] = ???
    implicit def catsKernelOrderForTuple10[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9](implicit A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9)] = ???
    implicit def catsKernelOrderForTuple9[A0, A1, A2, A3, A4, A5, A6, A7, A8](implicit A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8)] = ???
    implicit def catsKernelOrderForTuple8[A0, A1, A2, A3, A4, A5, A6, A7](implicit A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7]): Order[(A0, A1, A2, A3, A4, A5, A6, A7)] = ???
    implicit def catsKernelOrderForTuple7[A0, A1, A2, A3, A4, A5, A6](implicit A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6]): Order[(A0, A1, A2, A3, A4, A5, A6)] = ???
    implicit def catsKernelOrderForTuple6[A0, A1, A2, A3, A4, A5](implicit A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5]): Order[(A0, A1, A2, A3, A4, A5)] = ???
    implicit def catsKernelOrderForTuple5[A0, A1, A2, A3, A4](implicit A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4]): Order[(A0, A1, A2, A3, A4)] = ???
    implicit def catsKernelOrderForTuple4[A0, A1, A2, A3](implicit A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3]): Order[(A0, A1, A2, A3)] = ???
    implicit def catsKernelOrderForTuple3[A0, A1, A2](implicit A0: Order[A0], A1: Order[A1], A2: Order[A2]): Order[(A0, A1, A2)] = ???
    implicit def catsKernelOrderForTuple2[A0, A1](implicit A0: Order[A0], A1: Order[A1]): Order[(A0, A1)] = ???
    implicit def catsKernelOrderForTuple1[A0](implicit A0: Order[A0]): Order[Tuple1[A0]] = ???
  }

}