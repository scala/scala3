package test 

import annotation.{tailrec, switch}

object typers {
  
  object Eta {
    
    def fun(x: Int): Int = x + 1
    val foo = fun(_)
  }
  
  case class DefaultParams(init: String => String = identity)
  object DefaultParams {
    def foo(x: String => String = identity) = x("abc")
    
    foo()
  }
  
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
    
    println("abc".toString)

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
  
  object Accessibility {
    
    class A {
      val x: String = "abc"
    }
    
    class B extends A {
      private def x: Int = 1
    }
    
    val b: B = new B
    val y = b.x
    val z: String = y
    
  }
  
  object Self {
    
    class A(self: Int) { self =>
      
      class B {
        val b = self
        val c: A = b
      }
      
      val a = self
      val c: A = a
    }
    
    
  }

}