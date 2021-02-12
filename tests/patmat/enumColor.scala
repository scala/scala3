 enum Color {
   case Red, Green, Blue
 }

 object Test {
   def f(color: Color) = {
     import Color.*
     color match {
       case Red | Green | Blue =>
     }
  }
}
