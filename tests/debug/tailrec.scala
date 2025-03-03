object Test {
  def fact(x: Int): Int = {
    if (x == 0)
      1
    else
      x * fact(x - 1) // TODO: incorrect this line when x = 0
  }


   def main(args: Array[String]): Unit = {
     val a = 1 + 2
     val b = a * 9     // [break] [step: fact]
     val c = fact(a)   // [step: x == 0] [step: fact(x - 1)] [step: x == 0] [cont]
     fact(0)           // [break] [step: x == 0] [step: 1] [step: fact(x - 1)] [step: print]
     println(c)          // [cont]
  }
}