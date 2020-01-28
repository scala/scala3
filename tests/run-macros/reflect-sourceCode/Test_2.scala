import api._

object Test {
  def f(implicit x: Int): Int = x * x
  def main(args: Array[String]): Unit = {
    implicit val x: Int = 10
    assert(args(0).reflect == "args(0)")
    assert(args( 0 ).reflect == "args( 0 )")
    assert(args( 0 /* ignore */).reflect == "args( 0 /* ignore */)")
    assert(f.reflect == "f")
    assert((f).reflect == "f")
    assert( { f }.reflect == "{ f }")
    assert( { f; f }.reflect == "{ f; f }")
  }
}
