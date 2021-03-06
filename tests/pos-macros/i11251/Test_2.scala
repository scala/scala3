package i11251


object Main {

 def main(args:Array[String]):Unit =
    val sl = new SL()
    X.process(
      sl.foo(x=>x+1).run()
    )

}
