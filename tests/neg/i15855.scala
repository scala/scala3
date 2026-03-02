class MyFunction(args: String)

trait MyFunction0[+R] extends MyFunction {
  def apply(): R
}

def fromFunction0[R](f: Function0[R]): MyFunction0[R] = () => f() // error

class MyFunctionWithImplicit(implicit args: String)

trait MyFunction0WithImplicit[+R] extends MyFunctionWithImplicit {
  def apply(): R
}

def fromFunction1[R](f: Function0[R]): MyFunction0WithImplicit[R] = () => f() // error
