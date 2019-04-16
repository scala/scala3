object Test {
   def a: 1 | 2 = 1
   def b: 3 | 4 = a // error
   def c: 1 | 2 = 1
   def d: 1 = a // error
}

// Following examples are from issue #1551
object Test2 {
  type ABC = 'A' | 'B' | 'C'
  type A2F = ABC | 'D' | 'E' | 'F'

  def foo(x: A2F) = ()
  foo('F') // ok
  foo('G') // error
}

object Get2As1 {
  class OrIntroFn[T, U, TU >: T|U]{
    type V = TU
    def tuToV(): (TU => V) = p => p
  }
  class Or11X[X] extends OrIntroFn[1&1, X, (1&1|X)]{
    def get2as11X:V = tuToV()(2)  // error
  }
  class Or11Nothing extends Or11X[Nothing]
  def get2as1:1 = new Or11Nothing().get2as11X // error

  def main(a:Array[String]) = {
    println(get2as1) // prints 2
    val one:1 = get2as1
    println(one) // prints 1
  }
}