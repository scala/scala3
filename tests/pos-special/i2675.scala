class Ref[-T]

sealed trait Op
case class Send[T](ref: Ref[T], msg: T) extends Op

object Main {
  val ref: Ref[String] = ???
  val op: Op = Send(ref, "hello")
  op match {
    case Send(ref, msg) =>
  }
}

