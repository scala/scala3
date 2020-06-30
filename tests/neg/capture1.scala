// Test shows that Java soundness hole does not apply in Dotty
import collection.mutable
object Test extends App {

  val l: mutable.Seq[String] = mutable.ArrayBuffer()

  extension [T, U](xs: List[T]) def emap (f: T => U): List[U] = xs.map(f)

  extension [T](xs: List[T]) def ereduce (f: (T, T) => T): T = xs.reduceLeft(f)

  extension [T](xs: mutable.Seq[T]) def append (ys: mutable.Seq[T]): mutable.Seq[T] = xs ++ ys

  List(l, mutable.ArrayBuffer(1))
    .emap(list => list)
    .ereduce((xs, ys) => xs `append` ys) // error

}