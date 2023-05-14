implicit def catsSyntaxEq[A: Eq](a: A): Foo[A] = ???

class Foo[A]
object Foo:
  given [A: Eq]: Eq[Foo[A]] = ???

object FooT:

  def foo[X](using Order[X]): Unit = ???
  foo  // error

  def map[F[_], A](ffa: F[Foo[A]])(f: A): Nothing = ???

  given OrderFFooA[F[_], A](using Ord: Order[F[Foo[A]]]): Order[F[Foo[A]]] = ???

trait Eq[A]
trait Order[A] extends Eq[A]

object Eq {
  given catsKernelOrderForTuple1[A0](using A0: Order[A0]): Order[Tuple1[A0]] = ???
  given catsKernelOrderForTuple2[A0, A1](using A0: Order[A0], A1: Order[A1]): Order[(A0, A1)] = ???
  given catsKernelOrderForTuple3[A0, A1, A2](using A0: Order[A0], A1: Order[A1], A2: Order[A2]): Order[(A0, A1, A2)] = ???
  given catsKernelOrderForTuple4[A0, A1, A2, A3](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3]): Order[(A0, A1, A2, A3)] = ???
  given catsKernelOrderForTuple5[A0, A1, A2, A3, A4](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4]): Order[(A0, A1, A2, A3, A4)] = ???
  given catsKernelOrderForTuple6[A0, A1, A2, A3, A4, A5](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5]): Order[(A0, A1, A2, A3, A4, A5)] = ???
  given catsKernelOrderForTuple7[A0, A1, A2, A3, A4, A5, A6](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6]): Order[(A0, A1, A2, A3, A4, A5, A6)] = ???
  given catsKernelOrderForTuple8[A0, A1, A2, A3, A4, A5, A6, A7](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7]): Order[(A0, A1, A2, A3, A4, A5, A6, A7)] = ???
  given catsKernelOrderForTuple9[A0, A1, A2, A3, A4, A5, A6, A7, A8](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8)] = ???
  given catsKernelOrderForTuple10[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9)] = ???
  given catsKernelOrderForTuple11[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9], A10: Order[A10]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)] = ???
  given catsKernelOrderForTuple12[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9], A10: Order[A10], A11: Order[A11]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)] = ???
  given catsKernelOrderForTuple13[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9], A10: Order[A10], A11: Order[A11], A12: Order[A12]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)] = ???
  given catsKernelOrderForTuple14[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9], A10: Order[A10], A11: Order[A11], A12: Order[A12], A13: Order[A13]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)] = ???
  given catsKernelOrderForTuple15[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9], A10: Order[A10], A11: Order[A11], A12: Order[A12], A13: Order[A13], A14: Order[A14]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)] = ???
  given catsKernelOrderForTuple16[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9], A10: Order[A10], A11: Order[A11], A12: Order[A12], A13: Order[A13], A14: Order[A14], A15: Order[A15]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)] = ???
  given catsKernelOrderForTuple17[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9], A10: Order[A10], A11: Order[A11], A12: Order[A12], A13: Order[A13], A14: Order[A14], A15: Order[A15], A16: Order[A16]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)] = ???
  given catsKernelOrderForTuple18[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9], A10: Order[A10], A11: Order[A11], A12: Order[A12], A13: Order[A13], A14: Order[A14], A15: Order[A15], A16: Order[A16], A17: Order[A17]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)] = ???
  given catsKernelOrderForTuple19[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9], A10: Order[A10], A11: Order[A11], A12: Order[A12], A13: Order[A13], A14: Order[A14], A15: Order[A15], A16: Order[A16], A17: Order[A17], A18: Order[A18]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)] = ???
  given catsKernelOrderForTuple20[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9], A10: Order[A10], A11: Order[A11], A12: Order[A12], A13: Order[A13], A14: Order[A14], A15: Order[A15], A16: Order[A16], A17: Order[A17], A18: Order[A18], A19: Order[A19]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)] = ???
  given catsKernelOrderForTuple21[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9], A10: Order[A10], A11: Order[A11], A12: Order[A12], A13: Order[A13], A14: Order[A14], A15: Order[A15], A16: Order[A16], A17: Order[A17], A18: Order[A18], A19: Order[A19], A20: Order[A20]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)] = ???
  given catsKernelOrderForTuple22[A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](using A0: Order[A0], A1: Order[A1], A2: Order[A2], A3: Order[A3], A4: Order[A4], A5: Order[A5], A6: Order[A6], A7: Order[A7], A8: Order[A8], A9: Order[A9], A10: Order[A10], A11: Order[A11], A12: Order[A12], A13: Order[A13], A14: Order[A14], A15: Order[A15], A16: Order[A16], A17: Order[A17], A18: Order[A18], A19: Order[A19], A20: Order[A20], A21: Order[A21]): Order[(A0, A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)] = ???
}
