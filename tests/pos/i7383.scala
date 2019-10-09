trait ZSink {

  type State

  def foo: ZSink =
    class Anon extends ZSink
      case class State(x: Int)
    new Anon
//    new ZSink {
//      case class State(x: Int)
//    }
}