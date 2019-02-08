object Test {
  val a =
    try null.asInstanceOf[Nothing]
    catch { case e: ClassCastException if e.getMessage == "Cannot cast to scala.Nothing" => /* As expected */ }
  def main(args: Array[String]): Unit = {
  }
}
