// Test shows that Java soundness hole does not apply in Dotty
import collection.mutable
object Test extends App {

  val l: mutable.Seq[String] = mutable.ArrayBuffer()

  def (xs: List[T]) emap[T, U] (f: T => U): List[U] = xs.map(f)

  def (xs: List[T]) ereduce[T] (f: (T, T) => T): T = xs.reduceLeft(f)

  def (xs: mutable.Seq[T]) append[T] (ys: mutable.Seq[T]): mutable.Seq[T] = xs ++ ys

  List(l, mutable.ArrayBuffer(1))
    .emap(list => list)
    .ereduce((xs, ys) => xs `append` ys) // error

}