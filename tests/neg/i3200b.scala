object Test {
  def main(args: Array[String]): Unit = {
    val a: Nil.type = (Vector(): Any) match { case Nil as n => n } // error
    val b: Nil.type = (Vector(): Any) match { case (Nil as m) as n => n } // error
    val c: Int = (1.0: Any) match { case 1 as n => n } // error
    val d: Int = (1.0: Any) match { case (1 as m) as n => n } // error
  }
}
