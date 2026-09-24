case class State(text: String):
  val updateText1 = copy(text = _) // error
  val updateText2 = copy(text = (_: String)) // error
