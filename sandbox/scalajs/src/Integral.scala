package hello

// Minimal reproduction
object Integral {
  def main(args: Array[String]): Unit = {
    trait A[T] extends Ordering[T]
    case class B[T](v: T) extends A[T] {
      def compare(x: T, y: T): Int = 0
    }

    B(0)
  }
}
