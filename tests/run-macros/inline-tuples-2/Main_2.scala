
object Test {
  import Macros._

  def main(args: Array[String]): Unit = {
    println(get1(Tuple1(1)))
    println(get1(new Tuple1(2)))

    println(get2(4))
    println(get3(5))

    println(get4(Tuple1(Tuple1(6))))
  }

}
