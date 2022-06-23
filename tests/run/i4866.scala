// scalajs: --skip

// Test that try-finally aren't lifted, but try-catch are.

class Foo {
  val field = try 1 finally ()
}

class FooLifted {
  val field = try 1 catch { case e: Exception => () } finally () 
}

object Test extends App {
  def numLifted(o: Object) = {
    def isLifted(m: java.lang.reflect.Method) = m.getName.startsWith("lifted")
    o.getClass.getDeclaredMethods.count(isLifted)
  }
  
  println("Foo #lifted: " + numLifted(new Foo))
  println("FooLifted #lifted: " + numLifted(new FooLifted))
}
