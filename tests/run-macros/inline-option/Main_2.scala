
object Test {

  import Macros._

  def main(args: Array[String]): Unit = {
    println(size(None))
    println(size(Some(1)))
    println(size(new Some(2)))

    println(size2(3))
    println(size3(4))
    println(size4(5))

    println(size5(None))
    println(size5(Some(None)))
    println(size5(Some(Some(6))))
  }

}
