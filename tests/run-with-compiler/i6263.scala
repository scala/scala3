import quoted._
object Test {

  def main(args: Array[String]): Unit = {
    fn("foo")
    fn((1,2))
    fn(O)
    fn(1)
  }

  def fn[T : Type](v : T) = "ok"
}

object O
