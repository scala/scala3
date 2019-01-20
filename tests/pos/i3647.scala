object App {
  def main(args: Array[String]): Unit = {
    trait FooT {
      type T
      type Bar[A]

      def get(k: Bar[T]): String
    }
    val test: FooT = new FooT {
      type T = String
      type Bar[A] = J[A]
      sealed abstract class J[A]
      case object JName extends J[T]
      case object JInt extends J[Int]

      def get(k: J[T]): String = k match {
        case JName => "Age"
      }
    }
  }
}
