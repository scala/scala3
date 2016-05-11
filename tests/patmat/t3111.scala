object Test {
  val bool: Boolean = false

  bool match {
    case true => "true!"
  }

  bool match {
    case true => "true!"
    case false => "false!"
    case _ => "cats and dogs living together... mass hysteria!"
  }
}