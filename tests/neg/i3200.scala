object Test {
  case object Bob { override def equals(other: Any) = true }
  def main(args: Array[String]): Unit = {
    val m : Bob.type = (5: Any) match { case x @ Bob => x }  // error
  }
}
