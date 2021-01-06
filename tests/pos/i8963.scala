type Numeric = Double | Int

sealed trait VarValues[T, C <: VarValues[T,C]] {
  this: C =>
  val arr: Array[T]
}
final case class VarDoubles(arr: Array[Double]) extends VarValues[Double, VarDoubles]
final case class VarInts(arr: Array[Int]) extends VarValues[Int, VarInts]
final case class VarStrs(arr: Array[String]) extends VarValues[String, VarStrs]

def check7(a: VarValues[_,_], b: VarValues[_,_]): Unit = {
  (a,b) match {
    case (x:(VarDoubles|VarInts), y:(VarDoubles|VarInts)) =>
      val x0: Iterator[Numeric] = x.arr.iterator
      val y0: Iterator[Numeric] = y.arr.iterator
      val l0: Iterator[(Numeric, Numeric)] = x0.zip(y0)
      val ll0: Iterator[(Numeric, Numeric)]#GroupedIterator[(Numeric, Numeric)] = x0.zip(y0).sliding(2,1)
      ???
    case _ => ???
  }
}