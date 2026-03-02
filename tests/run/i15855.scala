class MyFunction(args: String*)

trait MyFunction0[+R] extends MyFunction {
  def apply(): R
}

abstract class MyFunction1[R](args: R*):
  def apply(): R

def fromFunction0[R](f: Function0[R]): MyFunction0[R] = () => f()
def fromFunction1[R](f: Function0[R]): MyFunction1[R] = () => f()

@main def Test =
  val m0: MyFunction0[Int] = fromFunction0(() => 1)
  val m1: MyFunction1[Int] = fromFunction1(() => 2)
  assert(m0() == 1)
  assert(m1() == 2)
