// crash.scala
import scala.language.implicitConversions

class MyFunction(args: String)

trait MyFunction0[+R] extends MyFunction {
  def apply(): R
}

def fromFunction0[R](f: Function0[R]): MyFunction0[R] = () => f() // error
