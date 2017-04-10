 enum Color {
   case Red, Green, Blue
 }

 object Test {
   def f(color: Color) = {
     import Color._
     color match {
       case Red | Green | Blue =>
     }
  }
}
