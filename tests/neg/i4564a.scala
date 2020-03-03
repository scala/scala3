
class BaseCP[T] {
  def apply(x: T): ClashPoly = if (???) ClashPoly(1) else ???
}
object ClashPoly extends BaseCP[Int]
case class ClashPoly private(x: Int) // private method apply cannot override method apply in class BaseCP