sealed trait HList
sealed trait HNil extends HList
sealed trait ::[+H, +T <: HList] extends HList

case class Size[L <: HList](value: Int)

object Size {
  implicit val caseHNil: Size[HNil] = Size(0)
  implicit def caseHCons[H, T <: HList](implicit e: Size[T]): Size[H :: T] = Size(e.value + 1)
}

object HListTest {
  def main(args: Array[String]): Unit = {
    assert(implicitly[Size[HNil]].value == 0)
    assert(implicitly[Size[Int :: HNil]].value == 1)
    assert(implicitly[Size[Int :: Int :: HNil]].value == 2)
    assert(implicitly[Size[Int :: Int :: Int :: HNil]].value == 3)
  }
}
