object Test {
   def main(args: Array[String]): Unit = {
     val a = 1 + 2     // [break] [step: a * 9]
     val b = a * 9     // [step: plus] [step: x * x]

     def plus(x: Int, y: Int) = {
       val a = x * x               // [step: y * y]
       val b = y * y               // [step: a + b]
       a + b                       // [step: plus]
     }

     val c = plus(a, b)            // [step: print] [cont]
     println(c)
  }
}