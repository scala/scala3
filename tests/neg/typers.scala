object typers {
  
  class C {
    val x: Int
    val x: String
    val y: Int
    def y: String
    val z: Int
    def z(): String
    
    def f(x: Any) = ()
    def f(x: AnyRef): AnyRef
    
    def g(x: Object): Unit
    def g[T](x: T): T = x
  }
  
  
  object returns {
    
    def foo(x: Int) = {
      return 3
    }
    
    return 4
  }
  
  object cyclic {
    def factorial(acc: Int, n: Int) = 
      if (n == 0) acc
      else factorial(acc * n, n - 1)
      
    def foo(x: Int) = x
    def foo() = foo(1)
      
  }
  
  object tries {

    val x = try {
      "abc"
    } catch {
      case ex: String => // does not work yet. We should detect that the test is non-sensical, but don't.
        123
    }
  }

  class Refinements {
    val y: C { val x: T; type T }
  }
}