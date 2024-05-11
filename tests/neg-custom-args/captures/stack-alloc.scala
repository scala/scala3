import scala.collection.mutable

class Pooled

val stack = mutable.ArrayBuffer[Pooled]()
var nextFree = 0

def withFreshPooled[T](op: (lcap: caps.Cap) ?-> Pooled^{lcap} => T): T =
  if nextFree >= stack.size then stack.append(new Pooled)
  val pooled = stack(nextFree)
  nextFree = nextFree + 1
  val ret = op(using caps.cap)(pooled)
  nextFree = nextFree - 1
  ret

def test() =
  val pooledClosure = withFreshPooled(pooled => () => pooled.toString) // error
  pooledClosure()