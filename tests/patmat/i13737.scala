sealed trait Result

case class Success(result: String, next: Int) extends Result {
  def isEmpty: Boolean = 10 % 2 == 1
  def get: String = result
}

object Success {
  def unapply(x: Success): Success = x
}

def main =
  val res: Result = ???
  res match // error
    case Success(v) => v
