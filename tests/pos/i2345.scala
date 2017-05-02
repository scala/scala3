import scala.collection.mutable

abstract class Whatever[A] extends mutable.Set[A] {
  private[this] var count = 0
  count = 0
}
