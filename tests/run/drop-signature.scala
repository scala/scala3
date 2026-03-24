// based on scala.collection.immutable.*

transparent trait IterableOps[+A, +CC[_], +C] extends Any {
  def drop(n: Int): C = ???
}

trait Iterable[+A] extends IterableOps[A, Iterable, Iterable[A]]

abstract class AbstractIterable[+A] extends Iterable[A]

class Map[K, +V] extends AbstractIterable[(K, V)]


object Test:
  def main(args: Array[String]): Unit =
    val m = new Map[Int, String]()
    m.getClass.getMethods.filter(_.getName == "drop").map(_.toGenericString).foreach(println)
