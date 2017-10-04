trait HList
trait HNil extends HList

trait FromTraversable[Out <: HList]
object FromTraversable {
  implicit def hnilFromTraversable[T]: FromTraversable[HNil] =
    new FromTraversable[HNil]{}
}

object Filter {
  def apply[A <: HList, O <: HList]()(implicit ftA: FromTraversable[A],
                                      ftO: FromTraversable[O]): Unit = ()
}
object Main {
  def main = Filter[HNil, HNil]()
}
