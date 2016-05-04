import scala.collection.mutable._
class X(val x: Int) extends AnyVal
object Test {
  def main(args: Array[String]) = {
    val ar3 = Array(Array(new X(1)), Array(new X(2)))
    val ar = Array(new X(1), new X(2))
    println(ar.deep) //ArrayLike
    println(ar.view) //IndexedSeqLike
    println(ar.view(0,1))
    println(ar.drop(0).toList)
    println(ar.take(0).toList)
    println(ar.iterator)
    println(ar.toBuffer)
    println(ar.head)
    println(ar.seq)
    println(ar3.flatten.toList)

    val ar2: WrappedArray[X] = ar
    println(ar2.toArray.toSet)
    ar2.companion

    val t = Array((1, new X(3)), (2, new X(4)))
    t.unzip

    val t2: Array[Array[X]] = Array(Array(new X(1), new X(2), new X(3)), Array(new X(4), new X(5), new X(6)))
    val t3: WrappedArray[Array[X]] = t2
    println(t3.transpose.toSet)
    val t4: ArrayOps[Array[X]] = t2
    println(s"t4.transpose: ${t4.transpose.toList}")
    val t21: Array[Array[X]] = Array(Array[X](), Array[X]())
    println(s"t21.transpose: ${t21.transpose.toList}")
  }
}