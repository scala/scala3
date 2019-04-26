
object Test {
  import Macro._

  def main(args: Array[String]): Unit = {
    val ls = List(1, 2, 3)
    optimize(ls.filter(x => x < 3).filter(x => x > 1))
    optimize(ls.filter(x => x < 3).filter(x => x > 1).filter(x => x == 2))
    optimize(ls.filter(x => x < 3).foreach(x => println(x)))
    optimize(List(1, 2, 3).map(a => a * 2).map(b => b.toString))
  }

}
