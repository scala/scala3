package example

class CaseTest {
  def foo (x: Option[Int]) : Int =
  x match {
    case y @ Some(x) => x
    case None => 0
  }
}

case class CaseClass(x: Int)
case object CaseObject