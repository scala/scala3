package ccbug

import language.experimental.captureChecking

class ListMonadContext extends CpsMonadContext[List]

object ListMonad extends CpsMonad[List]:
  type Context = ListMonadContext
  def apply[T](op: Context ?=> T): List[T] =
    List(op(using new ListMonadContext))

given listMonad: ListMonad.type = ListMonad

def test() =
  val r = async[List] { 42 }
  r
