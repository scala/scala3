inline trait C[S]:
   def v(x: S): S = x
   def w: Unit = 
      val x = new C[S] {} // error: May not inline an inline trait into a class defined inside another inline trait. If you really need to do this, make the inline trait Specialized or move the class definition outside the trait.
      println("w")

class B extends C[Char]
