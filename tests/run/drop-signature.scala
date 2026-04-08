// scalajs: --skip
// (JVM-only test about signatures)

// based on scala.collection.immutable.*

transparent trait IterableOps[+A, +CC[_], +C] extends Any {
  def drop(n: Int): C = ???
}

type AnyConstr[X] = Any

transparent trait MapOps[K, +V, +CC[_, _] <: IterableOps[?, AnyConstr, ?], +C] extends IterableOps[(K, V), Iterable, C]

trait Iterable[+A] extends IterableOps[A, Iterable, Iterable[A]]

abstract class AbstractIterable[+A] extends Iterable[A]

trait Map[K, +V] extends Iterable[(K, V)] with MapOps[K, V, Map, Map[K, V]]

abstract class AbstractMap[K, +V] extends AbstractIterable[(K, V)] with Map[K, V]

object MMM extends AbstractMap[Int, String]

object Test:
  def main(args: Array[String]): Unit =
    val m: Map[Int, String] = MMM
    m.getClass.getMethods.filter(_.getName == "drop").map(_.toGenericString).foreach(println)
