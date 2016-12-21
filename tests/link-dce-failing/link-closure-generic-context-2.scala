object Test {
  class A[T] {
    def f(y: T): T = ((x: T) => x)(y)
  }
  
  class B[T] extends A[T]
  
  def main(args: Array[String]): Unit = System.out.println((new B[Int]).f(42))
}