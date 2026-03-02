import scala.language.experimental.erasedDefinitions

class EC extends compiletime.Erased

trait X {
  def m1(using i: Int): Int
  def m2(erased i: Int): Int
  def m3(i: Int, erased j: Int): Int
  def m4(i: EC): Int

  val l1 = (x: Int) ?=> 5
  val l2 = (erased x: Int) => 5
  val l3 = (erased x: Int) ?=> 5
  val l4 = (x: Int, erased y: Int) => 5
  val l5 = (x: EC) => 5
}

@main def Test = {
  println(inspect[X])
}
