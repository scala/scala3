// Test shows that Java soundness hole does not apply in Dotty
import collection.mutable
object Test extends App {

  val l: mutable.Seq[String] = mutable.ArrayBuffer()

  def [T, U](xs: List[T]) emap (f: T => U): List[U] = xs.map(f)

  def [T](xs: List[T]) ereduce (f: (T, T) => T): T = xs.reduceLeft(f)

  def [T](xs: mutable.Seq[T]) append (ys: mutable.Seq[T]): mutable.Seq[T] = xs ++ ys

  List(l, mutable.ArrayBuffer(1))
    .emap(list => list)
    .ereduce((xs, ys) => xs `append` ys) // error

}