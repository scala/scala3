import scala.collection.mutable

class Pooled

class Test extends caps.Stateful:
  val stack = mutable.ArrayBuffer[Pooled]()
  var nextFree = 0

  update def withFreshPooled[T](op: (lcap: caps.Capability) ?-> Pooled^{lcap} => T): T =
    if nextFree >= stack.size then stack.append(new Pooled)
    val pooled = stack(nextFree)
    nextFree = nextFree + 1
    val ret = op(using caps.any)(pooled)
    nextFree = nextFree - 1
    ret

  update def test() =
    val pooledClosure = withFreshPooled(pooled => () => pooled.toString) // error
    pooledClosure()