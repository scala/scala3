trait Ord[A] { def less(x: A, y: A): Boolean }

object Main {
  implicit val intOrd: Ord[Int] = new Ord {  // omitted [Int] here
    def less(x: Int, y: Int) = x < y
  }

  def main(args: Array[String]): Unit = ()
}
