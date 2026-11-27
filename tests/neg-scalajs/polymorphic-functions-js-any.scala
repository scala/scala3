import scala.scalajs.js

def test =
  val f: [A] => js.Any = [A] => (x: Int) => x + 1 // error
