object A {
   def x = 3

   def y = {
     import B._
     x  // error: ambiguous
   }
}
object B {
  def x = 3
}
