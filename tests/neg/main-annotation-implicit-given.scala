object myProgram:
  implicit val x: Int = 2
  given Int = 3

  @main def showImplicit(implicit num: Int): Unit = // error
    println(num)

  @main def showUsing(using num: Int): Unit = // error
    println(num)

end myProgram
