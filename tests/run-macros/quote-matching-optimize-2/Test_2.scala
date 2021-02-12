object Test {
  import Macro.*

  def main(args: Array[String]): Unit = {
    val ls = List(1, 2, 3)
    val ls2 = List('a', 'b', 'c')
    optimize(ls.filter(x => x < 3).filter(x => x > 1))
    optimize(ls2.filter(x => x < 'c').filter(x => x > 'a'))
    optimize(ls.filter(x => x < 3).filter(x => x > 1).filter(x => x == 2))
    optimize(ls.filter(x => x < 3).foreach(x => println(x)))
    optimize(ls.map(a => a * 2).map(b => b.toString))
    optimize(ls.map(a => a.toChar).map(b => b.toString))
  }

}
