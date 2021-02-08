class FailHere

trait TC[T]
object TC {

  inline def mkDefaultTC[A]: TC[A] = inline compiletime.erasedValue[A] match {
    case _: FailHere => compiletime.error("blow up here")
    case _           => ???
  }

  inline given [T]: TC[T] = mkDefaultTC[T]
}

def test =
  summon[TC[Int]] // ok
  summon[TC[FailHere]] // error: blow up here
  TC.mkDefaultTC[FailHere] // error: blow up here
