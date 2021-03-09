
object Test {
  def main(args : Array[String]) : Unit =
    printInfos {
      val a: Int = 1
      var b: Int = 1
      def c: Int = 1
      def f1(): Int = 1
      def f2(i: Int): Int = 1
      def f3(i: Int)(j: Int): Int = 1
      def f4[T](x: T): Int = 1
      def f5(i: Int)(using Int): Int = 1
      type T1
      type T2 = Int
      type T3 <: Int
      type T4[X]
      object A
      trait A
      class B[X] extends A
    }
}

