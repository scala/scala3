object Test {
  class A[T] {
    def f(y: T): T = ((x: T) => x)(y)
  }
  
  def main(args: Array[String]): Unit = System.out.println((new A[Int]).f(42))
}