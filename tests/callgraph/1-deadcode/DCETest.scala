import scala.annotation.internal.DoNotDCE

object DCETest {

  def foo() = 2
  def bar() = foo
  
  @DoNotDCE def dceTest: Unit = {
    Test.shouldDCE(bar())
    foo()
  }
  
  def main(args: Array[String]): Unit = {
    foo()
  }
}
