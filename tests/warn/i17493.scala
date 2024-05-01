//> using options -explain
 class A(val s: String) extends AnyVal {
   def g = synchronized { println("hello, world") } // warn
 }
