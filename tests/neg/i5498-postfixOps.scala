import scala.concurrent.duration.*

def test() = {
  1 second // error: usage of postfix operator

  Seq(1, 2).filter(List(1,2) contains) // error: usage of postfix operator // error
}
