import annotation.{tailrec, switch}

object typers {

  class List[+T] {
    def :: (x: T) = new :: (x, this)
    
    def len: Int = this match {
      case x :: xs1 => 1 + xs1.len
      case Nil => 0
    }
  }
  
  object Nil extends List[Nothing]
  
  case class :: [+T] (hd: T, tl: List[T]) extends List[T]
  
  def len[U](xs: List[U]): Int = xs match {
    case x :: xs1 => 1 + len(xs1)
    case Nil => 0
  }
  
  object returns {
    
    def foo(x: Int): Int = {
      return 3
    }
  }
  
  object tries {

    val x = try {
      "abc"
    } catch {
      case ex: java.io.IOException =>
        123
    } finally {
      println("done")
    }

    val y = try 2 catch Predef.identity

    val z = try 3 finally "abc"

  }

  class C {
    
    @tailrec def factorial(acc: Int, n: Int): Int = (n: @switch) match {
      case 0 => acc
      case _ => factorial(acc * n, n - 1)
    }
      
    println(factorial(1, 10))
    
    
  }
  
  class Refinements {
    val y: C { type T; val key: T; def process(x: T): Int }
  }

}