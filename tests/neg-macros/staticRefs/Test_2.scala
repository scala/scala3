object Test {

  def main(args: Array[String]): Unit = {
    println(findClass("scala.Xyz")) // error
    println(findPackage("scala.xyz")) // error
    println(findModule("scala.Xyz")) // error
    println(findMethod("scala.Option.xyx")) // error
  }

}
