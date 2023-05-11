trait Wrapper[A1] {
  def use(a: A1 => Unit): Unit
}

trait Assertion[A2] {}

def hideTypeInOut[A3](
    c1: A3
)(using
    hider: HideAInOut[A3]
): Wrapper[hider.Out] = ???

def entryPoint(): Unit = {
  hideTypeInOut(1L)
    .use((pair) => myAssert(pair)(someAssertion(2))) // error Assertion[Int] instead of Assertion[Long]
}

private def myAssert[A4](a: A4)(assertion: Assertion[A4]): Unit = ()

private def someAssertion(i: Int): Assertion[Int] = ???

trait HideAInOut[-A] {
  type Out
  def get(left: A): Out
}

object HideAInOut {

  type Out[HideA, HideB] = HideAInOut[HideA] { type Out = HideB }

  given [GivenA]: HideAInOut.Out[GivenA, GivenA] =
    new HideAInOut[GivenA] {
      type Out = GivenA
      def get(left: GivenA): Out = left
    }
}
