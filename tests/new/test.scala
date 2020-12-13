
trait SemiGroup[T] {
  extension (x: T) def combine(y: T): T
}
trait Monoid[T] extends SemiGroup[T] {
  def unit: T
}
def sum[T: Monoid](xs: List[T]): T =
  xs.foldLeft(implicitly[Monoid[T]].unit)((x, y) => x.combine(y))
