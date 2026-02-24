import scala.collection.mutable

class Pooled

class Test extends caps.Stateful:
  val stack = mutable.ArrayBuffer[Pooled]()
  var nextFree = 0

  update def withFreshPooled[T](op: Pooled^ => T): T =
    if nextFree >= stack.size then stack.append(new Pooled)
    val pooled = stack(nextFree)
    nextFree = nextFree + 1
    val ret = op(pooled)
    nextFree = nextFree - 1
    ret

  update def test() =
    withFreshPooled(pooled => pooled.toString)