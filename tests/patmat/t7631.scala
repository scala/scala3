sealed trait Test
case class TestA() extends Test
case class TestB() extends Test

object Tester {
  val input : Test = TestA()
  val num = 3
  val x = input match {
    case TestA() if num == 3 => 2
  }
}