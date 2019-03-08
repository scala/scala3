import scala.collection.mutable

case class Foo[K, V]()(implicit conv: Conversion[V, Ordered[V]])
extends mutable.HashMap[K,V] {

  val a = this.toSeq.sortWith { case ((_, v1), (_, v2)) => v1 > v2 }

  val b = this.toSeq.sortWith(_._2 > _._2)
}
