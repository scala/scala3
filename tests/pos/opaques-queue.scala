class Elem
trait QueueSignature with
  type Queue
  def empty: Queue
  def append(q: Queue, e: Elem): Queue
  def pop(q: Queue): Option[(Elem, Queue)]
val QueueModule: QueueSignature =
  object QueueImpl extends QueueSignature with
    type Queue = (List[Elem], List[Elem])
    def empty = (Nil, Nil)
    def append(q: Queue, e: Elem): Queue = (q._1, e :: q._2)
    def pop(q: Queue): Option[(Elem, Queue)] = q match
      case (Nil, Nil) => None
      case (x :: xs, ys) => Some((x, (xs, ys)))
      case (Nil, ys) => pop((ys.reverse, Nil))
  QueueImpl

object queues with
  opaque type Queue = (List[Elem], List[Elem])
  def empty = (Nil, Nil)
  def append(q: Queue, e: Elem): Queue = (q._1, e :: q._2)
  def pop(q: Queue): Option[(Elem, Queue)] = q match
    case (Nil, Nil) => None
    case (x :: xs, ys) => Some((x, (xs, ys)))
    case (Nil, ys) => pop((ys.reverse, Nil))
