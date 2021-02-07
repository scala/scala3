import scala.compiletime.testing._

object Test:
  implicit class ShouldWrapper(lhs: Boolean):
    def shouldBe(rhs: Boolean): Unit = ???

  typeChecks("class Bar") shouldBe true
