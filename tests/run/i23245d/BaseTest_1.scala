abstract class BaseTest {
  def genName(): String = "outerAccess"
  trait Fixture {
    lazy val service: Service = new Service {
      val a = genName()
      def doIt(a: String): Int = 0
    }
  }
}

trait Service {
  def doIt(a: String): Int
}
