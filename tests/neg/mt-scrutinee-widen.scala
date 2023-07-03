// We widen scrutinee's that are inline proxies
// But make sure that term refs in scrutinees are not widened in general

val x: Int = 42
val y: Int = 43
val z: Int = 44

type IsX[T] =
    T match
        case x.type => true
        case _ => false
def test = summon[IsX[y.type] =:= IsX[z.type]] // error

def test2 = summon[
  (
    y.type match
      case x.type => true
      case _ => false
  ) =:= (
    z.type match
          case x.type => true
          case _ => false
  )
] // error
