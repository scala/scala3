trait Suite {
  def run() = println("Suite")
}
trait SuiteMixin { this: Suite =>
  def run(): Unit
}
trait BeforeAndAfterAll extends SuiteMixin { this: Suite =>
  abstract override def run() = super.run()
}
class MySpec extends Suite with BeforeAndAfterAll
object Test {
  def main(args: Array[String]): Unit = {
    new MySpec().run()
  }
}