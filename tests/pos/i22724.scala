import java.util.concurrent.atomic.AtomicReference

object UnboundedHub:
  final class Node[A](var value: A, val pointer: AtomicReference[Pointer[A]])
  final case class Pointer[A](node: Node[A], subscribers: Int)

private final class UnboundedHub[A]:
  import UnboundedHub.*

  val publisherHead: AtomicReference[Node[A]] = new AtomicReference(
    new Node[A](
      null.asInstanceOf[A],
      new AtomicReference(
        Pointer(null, 0) // error: too many arguments for constructor AtomicReference
      )
    )
  )