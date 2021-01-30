object Test {
  def main(args: Array[String]): Unit = {
    val a: Nil.type = (Vector(): Any) match { case n @ Nil => n } // error
    val b: Nil.type = (Vector(): Any) match { case n @ (m @ Nil) => n } // error
    val c: Int = (1.0: Any) match { case n @ 1 => n } // error
    val d: Int = (1.0: Any) match { case n @ (m @ 1) => n } // error
  }
}
