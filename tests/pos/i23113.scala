//> using options -Werror

object Repro {
  sealed abstract class Listener

  final case class Empty()
    extends Listener

  final case class Waiting(next: Promise)
    extends Listener

  def foo[A](l: Listener): Unit = {
    l match {
      case Empty() => ()
      case w @ Waiting(_) => ()
    }
  }
}

sealed trait Promise

object Promise {

  def apply(): Promise = new PromiseImpl()

  private abstract class PromiseBase extends Promise

  private final class PromiseImpl() extends PromiseBase
}
