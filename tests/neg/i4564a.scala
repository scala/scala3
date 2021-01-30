abstract class BaseCP[T] {
  def apply(x: T): ClashPoly
}
object ClashPoly extends BaseCP[Int] // error: object creation impossible
case class ClashPoly private(x: Int) // error: private method apply cannot override method apply in class BaseCP