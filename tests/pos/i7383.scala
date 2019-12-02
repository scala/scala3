trait ZSink {

  type State

  def foo: ZSink =
    class Anon extends ZSink {
      case class State(x: Int)
    }
    new Anon

  def foo2: ZSink =
    new ZSink {
      case class State(x: Int)
    }
}