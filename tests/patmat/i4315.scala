import scala.concurrent.duration.{Duration, FiniteDuration}

class Test {
  def test(d: Duration) = d match {
    case finite: FiniteDuration =>
    case d =>
  }
}
