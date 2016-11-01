import scala.annotation.internal.DoNotDCE

object Main {

  def foo() = 2
  def bar() = foo
  
  @DoNotDCE
  def testEntry: Unit = {
    DCEUtils.shouldDCE(bar())
    foo()
  }
  
  def main(args: Array[String]): Unit = {
    foo
  }
}
