import scala.util.control.NonLocalReturns._

object Test {
  def has(xs: List[Int], elem: Int) =
    returning {
      for (x <- xs)
        if (x == elem) throwReturn(true)
      false
    }

  def main(arg: Array[String]): Unit = {
    assert(has(1 :: 2 :: Nil, 1))
    assert(has(1 :: 2 :: Nil, 2))
    assert(!has(1 :: 2 :: Nil, 3))
  }
}
