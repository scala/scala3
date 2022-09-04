object O {
  class AC(code: => Unit)

  val action = new AC({mode = ???}) {}

  def mode: AnyRef = ???
  def mode_=(em: AnyRef): Unit = {}
}