object Test {

  def main(args: Array[String]): Unit = {
    println(showMe"${1: Int} ${"abc": String}")
    println(showMe"${1} ${"abc"}")
    println(showMe"${1} ${println("xyz"); "xyz"}")
  }

}
