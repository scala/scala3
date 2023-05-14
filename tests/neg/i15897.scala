object O {
  class AC(code: => Unit)

  val action = new AC({mode = ???}) {} // error

  def mode: AnyRef = ???
  def mode=(em: AnyRef): Unit = {} // error // error // error
}