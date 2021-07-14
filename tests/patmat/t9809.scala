// scalac: -Ycheck-all-patmat
object Example {
  val op1: (Any, Any) => Unit = {
    case (_, b: Int) =>
  }

  val op2: (Unit, Any) => Unit = {
    case (_, b: Int) =>
  }
}
