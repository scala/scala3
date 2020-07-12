import scala.annotation.varargs

object VarArgs {
   @varargs
   def foo1[A](x: A, xs: String*): A = ???

   @varargs
   def foo2[A](x: List[A], xs: String*): A = ???

   @varargs
   def foo3[A](n: Int*): Unit = ???

   @varargs
   def bar1[A](x: A*): Unit = ???

   @varargs
   def bar2[A](x: A, xs: A*): A = ???

   @varargs
   def bar3[A <: Comparable[A]](xs: A*): A = ???
}
