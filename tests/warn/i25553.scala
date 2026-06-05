//> using options -Werror -Wvalue-discard

trait BaseKyoDataTest:
  type Assertion
  def assertionSuccess: Assertion
  transparent inline def typeCheck(inline code: String): Assertion = assertionSuccess
  def pendingUntilFixed(f: => Unit): Unit = f

  def test(): Unit =
    pendingUntilFixed {
      typeCheck("Abort.run(Abort.fail(new Test[Int]))"): Unit
    }

object Test extends BaseKyoDataTest:
  type Assertion = Int
  def assertionSuccess: Assertion = 1
