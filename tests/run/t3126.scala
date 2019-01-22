object Test {
  case class C(x: Int)
  val v1: Some[Int] | Null = null
  val v2: Some[Int] = null.asInstanceOf[Some[Int]]

  def main(args: Array[String]): Unit = {
    try C.unapply(null.asInstanceOf[C]) catch { case _: MatchError => }
    try (v1: @unchecked) match { case Some(1) => } catch { case _: MatchError => }
    try (v2: @unchecked) match { case Some(1) => } catch { case _: NullPointerException => }
  }
}

