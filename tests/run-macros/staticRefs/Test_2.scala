object Test {

  def main(args: Array[String]): Unit = {
    println(findClass("scala.Option"))
    println(findClass("Foo"))
    println(findPackage("scala.collection"))
    println(findModule("scala.Option"))
    println(findModule("Foo"))
  }

}

class Foo

object Foo
