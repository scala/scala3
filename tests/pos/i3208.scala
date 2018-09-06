trait X
trait Y

trait MyError

class Test {
  def test(xs: List[X]): List[X] = xs.collect { case y: Y => y }

  def test2(x: X) = x match {
    case y: Y =>
      val b: X = y
  }

  def test3 =
    try ???
    catch {
      case e: MyError =>
        throw e
    }
}
