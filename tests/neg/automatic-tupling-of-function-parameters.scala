
object Test {

  val f1: Tuple1[Int] => Int = (x: Int) => x // error
  val g1: Tuple1[Int] => Int = _ // error

}
