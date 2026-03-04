package ccbug

import language.experimental.captureChecking

class MyCap extends Cap

object ListMonad extends Monad[List]:
  type Ctx = MyCap
  def pure[T](t: T): List[T] = List(t)

given ListMonad.type = ListMonad

def test(): List[Int] =
  runner[List].run(42)
