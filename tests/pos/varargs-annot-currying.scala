import scala.annotation.varargs

object VarArgs {
   @varargs
   def two(a: Int)(b: String*): Nothing = ???

   @varargs
   def twoPrimitive(a: Int)(b: Int*): Nothing = ???

   @varargs
   def three(a3: Int)(b3: String)(c3: String*): Nothing = ???

   @varargs
   def threePrimitive(a3: Int)(b3: String)(c3: Int*): Nothing = ???

   @varargs
   def emptyOk()()(xs: String*): Nothing = ???
}
