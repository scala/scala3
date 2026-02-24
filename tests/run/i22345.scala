import scala.reflect.ClassTag

def makeSeq[T](args: T*): Seq[T] = args

@main def Test: Unit =
  val a: Array[(Int, String)] = Array[Int *: String *: EmptyTuple]()
  val b: Array[(Int, String)] = Array[Int *: String *: EmptyTuple]((1, "hello"))
  val c: Array[(Int, String)] = makeSeq[Int *: String *: EmptyTuple]().toArray
  assert(summon[ClassTag[(Int, String)]] == summon[ClassTag[Int *: String *: EmptyTuple]])
