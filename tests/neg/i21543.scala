object CompilerCrash {
  trait Scope {
    private type Event = String

    case class Cmd(events: List[Event])
  }

  new Scope {
    val commands = List(
      Cmd(List("1", "2"))
    )
  }
}