object Test {

  def isAType(arg: String): Unit = arg match {    
    case _ : "a" => println("an `a`")
    case _ => println("not `a`")
  }

  def main(args: Array[String]): Unit = {
    isAType("a")
    println(new String("a").isInstanceOf["a"])
    isAType(new String("a"))
  }

}
