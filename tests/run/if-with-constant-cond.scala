
object Test {

  def main(args: Array[String]): Unit = {
    if ({ println("cond1"); true }: true) println("then1") else println("else1")
    if ({ println("cond2"); false }: false) println("then2") else println("else2")
  }

}