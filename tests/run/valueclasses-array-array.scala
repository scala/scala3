class X[T](val x: T) extends AnyVal
object Test {
  def main(args: Array[String]) = {
    def pl(xs: Array[_]) = println(xs.toList)
    pl(Array.ofDim[X](4))
    pl(Array.ofDim[X](4,3))
    Array.ofDim[X](4,3,2)
    Array.ofDim[X](4,3,2,5)
    Array.ofDim[X](4,3,2,5,6)
    pl(Array.concat(Array[X](new X(1), new X(2)), Array[X](new X(3), new X(4))))
    import scala.collection.mutable.{ WrappedArray, ArrayBuilder }
    val ar: WrappedArray[X] = Array[X](new X(1), new X(2))
    ar.toArray
    val ar0: WrappedArray[X] = Array[X](new X(1), new X(2))
    val ar1: ArrayBuilder[X] = ArrayBuilder.make[X]()
    val ar2 = ar1.++=(ar0)
    pl(Array.fill(5)(new X(1)))
    pl(Array.fill(5,6)(new X(1)))
    Array.fill(5,6,7)(new X(1))
    Array.fill(5,6,7,8)(new X(1))
    Array.fill(5,6,7,8,9)(new X(1))

    pl(Array.tabulate(5)(x => new X(x)))
    pl(Array.tabulate(4,5)((x,y) => new X(x + y)))
    Array.tabulate(4,5,6)((x,y,z) => new X(x + y + z))
    Array.tabulate(4,5,6,7)((x,y,z,q) => new X(x + y + z + q))
    Array.tabulate(4,5,6,7,8)((x,y,z,q,w) => new X(x + y + z + q + w))

    pl(Array.iterate(new X(5), 3)(xx => new X(2*xx.x)))
    println(Array.unapplySeq(Array[X](new X(1), new X(2), new X(3))))
  }
}