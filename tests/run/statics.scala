import scala.annotation.static

class Foo{
  class Bar {
    def qwa =
     Bar.field
         // 0: invokestatic  #31                 // Method Foo$Bar$.field:()I
         // 3: ireturn
  }
  object Bar {
     @static
     val field = 1
   }
}

object Foo{
 @static
 def method = 1

 @static
 val field = 2

 @static
 var mutable = 3

 @static
 def accessor = field
}

object Test {
 import Foo._
 def main(args: Array[String]): Unit = {
   method + field + mutable + accessor
 }
}

class WithLazies{
  lazy val s = 1
  //         98: getstatic     #30                 // Field WithLazies$.OFFSET$0:J
}
