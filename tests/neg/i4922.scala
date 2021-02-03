class SQ {
  val isEmpty = false
  val get = Array(1)
}
object A {
  def unapplySeq(a: Int): SQ = new SQ
}

object Main {
  def main(args: Array[String]): Unit = {
    val seq: Seq[Int] = 2 match {
      case A(xs*) => xs // error // error
    }
  }
}
