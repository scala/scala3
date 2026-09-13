class Bug {
  def main(args: Array[String]) = {
    var msg: String = ???;
    val f:  PartialFunction[Any, Unit] = { case 42 => msg = "coucou" };
  }
}
