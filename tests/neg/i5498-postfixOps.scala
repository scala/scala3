import scala.concurrent.duration.*

def test() = {
  1 second // error // error

  Seq(1, 2).filter(List(1,2) contains) // error
}
