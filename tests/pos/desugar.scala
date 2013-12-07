object desugar {

  // variables
  var x: Int = 2
  var y = x * x
  
  { var z: Int = y }
 
  def foo0(first: Int, second: Int = 2, third: Int = 3) = first + second
  def foo1(first: Int, second: Int = 2)(third: Int = 3) = first + second
  def foo2(first: Int)(second: Int = 2)(third: Int = 3) = first + second
  
  object caseClasses { self =>
    trait List[+T] {
      def head: T
      def tail: List[T]
    }

    case class Cons[+T](val head: T, val tail: List[T]) extends List[T]

    object Cons {
      def apply[T](head: T): Cons[T] = apply(head, Nil)
    }

    case object Nil extends List[Nothing]
  }
  
  import caseClasses._
  
  object patDefs {
    
    val xs = Cons(1, Cons(2, Nil))
    
    val Cons(y, ys) = xs
  }
}