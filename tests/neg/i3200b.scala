object Test {
  def main(args: Array[String]): Unit = {
    val a : Nil.type = (Vector(): Any) match { case n @ Nil => n } // error
  }
}
