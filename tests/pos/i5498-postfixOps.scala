import scala.concurrent.duration.*

def test() = {
  // only OK since the defaultOptions in the TestConfiguration includes -language:postfixOps
  1 second

  Seq(1, 2).filter(List(1,2) contains)
}