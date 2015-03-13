class Bug {
  def main(args: Array[String]) = {
    var msg: String = null;
    val f:  PartialFunction[Any, Unit] = { case 42 => msg = "coucou" };
  }
}
